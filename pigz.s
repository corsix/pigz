|.arch x64
|.section code
|.globals glob_
|.actionlist actions
|.define CRC

// Different calling conventions between Win/x64 and POSIX/x64.
|.if WIN
|.define Rarg1, rcx
|.define Rarg2, rdx
|.define Rarg3, r8
|.else
|.define Rarg1, rdi
|.define Rarg2, rsi
|.define Rarg3, rdx
|.endif

// Register assignments for (the majority of) pigz_available
// The first letter indicates bit width: R=64, E=32, W=16, B=8
// Other registers are used transiently: rax, rbx, rcx, rdx, xmm0-3
|.define Rstate, rbp
|.define Elitmask, r12d  // state->litbits if BMI2 else ((1 << state->litbits) - 1)
|.define Edistmask, r13d // state->distbits if BMI2 else ((1 << state->distbits) - 1)
|.define Rbits, r8       // state->bits (only the low Enbits bits are valid)
|.define Ebits, r8d
|.define Wbits, r8w
|.define Bbits, r8b
|.define Rnbits, rsi
|.define Enbits, esi     // state->nbits
|.define Bnbits, r6b
|.define Rinput, r9      // state->input
|.define Rinend, r10     // state->inend
|.define Einend, r10d
|.define Binend, r10b
|.define Rwritepos, r14  // state->writepos
|.define Ewritepos, r14d
|.define Wwritepos, r14w
|.define Ecrc, edi       // state->crc
|.define Wcrc, di
|.define Bcrc, r7b
|.define Rwritegoal, r15 // state->readpos + PIGZ_READ_SIZE
|.define Ewritegoal, r15d
|.define Rcrc_table, r11 // &pigz_crc_table

// Ensure that at least 32 bits of input are available in Rbits
|.macro needbits
|  // Pre-conditions: Usual pigz_available stack frame and register assignments
|  // Clobbers: rax, rcx, rdx, xmm0-15
|  cmp Enbits, 32
|  jae >9
|  lea rax, [>9]
|  mov ecx, Enbits
|  cmp Rinend, Rinput
|  jb ->need_reader_bits
|  // The length of the instructions between here and "9:" must match the N in
|  // the "sub rax, N" done by need_reader_bits (currently N=16).
|  mov eax, [Rinput]
|  add Rinput, 4
|  shl rax, cl
|  add Enbits, 32
|  or Rbits, rax
|9:
|  // Post-condition: 32 <= Enbits < 64
|  // If this condition cannot be satisfied, pigz_available will return, and
|  // PIGZ_STATUS_EOF or PIGZ_STATUS_UNEXPETED_EOF will be reported.
|.endmacro

// Stack frame layout for pigz_available
typedef struct Stack {
|.if WIN
  void* homespace[4];
|.endif
  uint16_t nper_lit[16];
  uint16_t nper_dist[16];
  void* need_reader_bits_continuation;
  uint32_t sorted_dist[30 + 1];
  uint32_t pad[1];
} Stack;

// Alternative fields for a pigz_state when its distcodes field is not being used.
typedef struct AltState {
  char reserved[offsetof(pigz_state, distcodes) + 8];
  uint16_t offset_lit[16];
  uint8_t offset_dist[16];
  uint8_t codelengths[286 + 30];
  uint32_t sorted_lit[286 + 1];
} AltState;

// DynASM type mappings
|.type Stack, Stack, rsp
|.type State, pigz_state, Rstate
|.type AltState, AltState, Rstate

#define bad_bits(n) (n)
#define OFFSET_lit_ops (256*4*4)
#define OFFSET_dist_ops (OFFSET_lit_ops+32*4)

int asm_export(const char* name);
int asm_cfi(const char* fmt, ...);
int asm_cfi_startproc();
int asm_cfi_endproc();
int asm_cfi_push(int offset, int reg);
int asm_cfi_save(int offset, int reg);
int asm_cfi_sub(int offset, int delta);

void pigz_emit_asm(Dst_DECL) {
int i;
int cfa = 8;

|.code
|.align 16
|=>asm_export("pigz_available"):
|->pigz_available:
|=>asm_cfi_startproc():
// Pre-conditions: Rarg1=state, (rsp & 15) == 8
// Fast-path: Do not allocate a stack frame until we know that we need it
|  mov r9, State:Rarg1->writepos
|  mov rdx, State:Rarg1->readpos
|  cmp dx, r9w
|  movzx eax, r9w
|  mov r8d, PIGZ_WINDOW_SIZE
|  movzx edx, dx
|  cmova eax, r8d
|  sub eax, edx
|  jz >1
|  ret
|1:
|  test byte State:Rarg1->status, 0xC0
|  jz >1
|  or byte State:Rarg1->status, 0x80
|  ret
|1:
// End of fast-path: time to allocate the stack frame
// Known: state->readpos == state->writepos (i.e. time to uncompress some more input)
// Known: 0 <= state->status < 64 (i.e. no error has occurred)
// Pre-conditions: r9=state->writepos
|  push rbx
|=>asm_cfi_push(cfa += 8, 3):
|  push rbp
|=>asm_cfi_push(cfa += 8, 6):
|  mov Rstate, Rarg1
|  push r12
|=>asm_cfi_push(cfa += 8, 12):
|  push r13
|=>asm_cfi_push(cfa += 8, 13):
|  mov Rinend, State:Rarg1->inend
|  push r14
|=>asm_cfi_push(cfa += 8, 14):
|  mov Rwritepos, r9
|  mov Rinput, State:Rarg1->input
|  push r15
|=>asm_cfi_push(cfa += 8, 15):
|  sub rsp, sizeof(Stack)
|=>asm_cfi_sub(cfa += sizeof(Stack), sizeof(Stack)):
|.if WIN
|  mov [rsp+cfa], rsi
|=>asm_cfi_save(cfa, 4):
|  mov [rsp+cfa+8], rdi
|=>asm_cfi_save(cfa + 8, 5):
|.endif
|  mov Rbits, State->bits
|  movzx Enbits, byte State->nbits
|  mov Ecrc, State->crc
|  jmp ->load_Rcrc_table_status_dispatch

|->return_from_available:
// Pre-conditions: Usual pigz_available stack frame and register assignments
|  mov State->nbits, Bnbits
|  add rsp, sizeof(Stack)
cfa -= sizeof(Stack);
|.if not WIN
|=>asm_cfi("remember_state"):
|=>asm_cfi("def_cfa_offset %d", cfa):
|.endif
|  mov State->bits, Rbits
|  pop r15
cfa -= 8;
|.if not WIN
|=>asm_cfi("def_cfa_offset %d", cfa):
|.endif
|  mov State->writepos, Rwritepos
|  pop r14
cfa -= 8;
|.if not WIN
|=>asm_cfi("def_cfa_offset %d", cfa):
|.endif
|  mov State->crc, Ecrc
|  pop r13
cfa -= 8;
|.if not WIN
|=>asm_cfi("def_cfa_offset %d", cfa):
|.endif
|  mov State->inend, Rinend
|  pop r12
cfa -= 8;
|.if not WIN
|=>asm_cfi("def_cfa_offset %d", cfa):
|.endif
|  mov Rarg1, Rstate
|  pop rbp
cfa -= 8;
|.if not WIN
|=>asm_cfi("def_cfa_offset %d", cfa):
|.endif
|  mov State:Rarg1->input, Rinput
|  pop rbx
cfa -= 8;
|.if not WIN
|=>asm_cfi("def_cfa_offset %d", cfa):
|.endif
|.if WIN
|  mov rdi, [rsp+16]
|  mov rsi, [rsp+8]
|.endif
|  jmp ->pigz_available
|.if not WIN
|=>asm_cfi("restore_state"):
|.endif

{
  // Start of main decompression loop
  // NB: The loop entry point is ->fetch_compressed_main_loop

  |.align 16
  |->lit_not_lit:
  // Pre-conditions: KIND == 2 (i.e. either end-of-block or error)
  // Pre-conditions: ebx contains VAL
  |  test ebx, ebx
  |  jz ->fetch_next_block
  |->bad_bits:
  |  mov byte State->status, PIGZ_STATUS_BAD_BITS ^ 0x80
  |  jmp ->return_from_available
  for (i = 0; i < 2; ++i) {
    |  nop
  }
  |->lit_not_length:
  // Pre-conditions: flags set by "cmp cl, 64"
  // Pre-conditions: 1 <= KIND <= 2
  // Pre-conditions: cl contains (KIND << 6)
  // Pre-conditions: ebx contains VAL
  |  jnz ->lit_not_lit
  // Known: KIND == 1 (i.e. VAL is a literal value)
  |  movzx eax, Wwritepos
  |  add Rwritepos, 1
  |  mov [Rstate + eax*1 + offsetof(pigz_state, window)], bl
  |.if CRC
  |  xor bl, Bcrc
  |  shr Ecrc, 8
  |  xor Ecrc, [Rcrc_table + ebx*4]
  |.endif
  |->fetch_compressed_main_loop:
  // Pre-conditions: Usual pigz_available stack frame and register assignments
  |  cmp Rwritepos, Rwritegoal
  |  jae ->return_from_available
  |  needbits
  // Known: enough bits are available for any literal/length code
  |  mov ebx, Elitmask
  |  and ebx, Ebits
  {
    |->load_lit_code:
    // Pre-conditions: ebx is an index into state->litcodes
    // Replaces ebx with the VAL from litcodes
    |  mov eax, [Rstate + ebx*4 + offsetof(pigz_state, litcodes)]
    |  xor ebx, ebx
    |  movzx ecx, ah
    // Known: cl contains NBITS
    |  shr Rbits, cl
    |  sub Enbits, ecx
    |  inc ebx
    |  movzx ecx, al
    // Known: cl contains (KIND << 6) | NXBITS
    |  shl ebx, cl // NB: Ignores the high two bits of cl
    |  dec ebx
    |  shr eax, 16
    |  and ebx, Ebits
    |  add ebx, eax
    // Known: ebx contains VAL
    |  cmp cl, 192
    |  jae ->load_lit_code
  }
  |  sub Enbits, ecx
  |  shr Rbits, cl // NB: Ignores the high two bits of cl
  |  and Enbits, 63
  |  cmp cl, 64
  |  jae ->lit_not_length
  // Known: KIND == 0 (i.e. VAL is a length value)
  |  needbits
  // Known: enough bits are available for any distance code
  |  mov edx, Edistmask
  |  and edx, Ebits
  {
    |->load_dist_code:
    // Pre-conditions: ebx contains a length value
    // Pre-conditions: edx is an index into state->distcodes
    // Replaces edx with the VAL from distcodes
    |  mov eax, [Rstate + edx*4 + offsetof(pigz_state, distcodes)]
    |  xor edx, edx
    |  movzx ecx, ah
    // Known: cl contains NBITS
    |  shr Rbits, cl
    |  sub Enbits, ecx
    |  inc edx
    |  movzx ecx, al
    // Known: cl contains (KIND << 6) | NXBITS
    |  shl edx, cl // NB: Ignores the high two bits of cl
    |  dec edx
    |  shr eax, 16
    |  and edx, Ebits
    |  add edx, eax
    // Known: edx contains VAL
    |  cmp cl, 192
    |  jae ->load_dist_code
  }
  |  test cl, 192
  |  .byte 0x2E; jnz ->bad_bits
  // Known: KIND == 0 (i.e. VAL is a distance value)
  // Replace edx with (writepos - edx) & (PIGZ_WINDOW_SIZE - 1)
  |  neg rdx
  |  sub Enbits, ecx
  |  shr Rbits, cl
  |  add rdx, Rwritepos
  |  js ->bad_bits // Distance is greater than writepos
  |  movzx edx, dx
  |  movzx ecx, Wwritepos
  // Start of backref-copy loop
  // Invariants: ebx contains number of bytes remaining to copy
  // Invariants: ecx contains the window index of the next byte to write
  // Invariants: edx contains the window index of the next byte to read
  |  add Rwritepos, rbx
  |.if CRC
  |  movd xmm0, esi // Temporarily spill esi
  |.endif
  |  test bl, 3
  |  jz ->backref_copy4
  {
    |->backref_copy: // One-byte-at-a-time backref-copy loop (at most three iterations)
    |  .byte 0x40; movzx eax, byte [Rstate + edx*1 + offsetof(pigz_state, window)]
    |  inc dx
    |  mov [Rstate + ecx*1 + offsetof(pigz_state, window)], al
    |.if CRC
    |  xor al, Bcrc
    |  shr Ecrc, 8
    |.endif
    |  inc cx
    |.if CRC
    |  xor Ecrc, [Rcrc_table + eax*4]
    |.endif
    |  .byte 0x81, 0xEB, 0x01, 0x00, 0x00, 0x00 // sub ebx, dword 1
    |  .byte 0x2E; jz ->fetch_compressed_main_loop
    |  .byte 0x40; test ebx, 3
    |  jnz ->backref_copy
  }
  {
    |->backref_copy4: // Four-bytes-at-a-time backref-copy loop (at most 64 iterations)
    // Pre-conditions: 0 < ebx <= 256, (ebx & 3) == 0
    // The input might overlap the output, so reads from the window and writes to
    // the window are still done one byte at a time, but the unrolling massively
    // helps the CRC calculation (and also reduces ebx manipulations).
    for (i = 3; i >= 0; --i) {
      |  movzx eax, byte [Rstate + edx*1 + offsetof(pigz_state, window)]
      |  inc dx
      |  mov [Rstate + ecx*1 + offsetof(pigz_state, window)], al
      |.if CRC
      if (i) {
        |  xor al, Bcrc
        |  shr Ecrc, 8
      } else {
        |  xor eax, Ecrc
      }
      |.endif
      |  inc cx
      |.if CRC
      if (i == 3) {
        |  mov esi, [Rcrc_table + eax*4 + 256*4*i]
      } else if (i) {
        |  xor esi, [Rcrc_table + eax*4 + 256*4*i]
      } else {
        |  xor esi, [Rcrc_table + eax*4]
        |  mov Ecrc, esi
      }
      |.endif
    }
    |  sub ebx, 4
    |  jnz ->backref_copy4
  }
  |.if CRC
  |  movd esi, xmm0 // Restore esi (it was spilled before the loop)
  |.endif
  // End of backref-copy loop
  |  jmp ->fetch_compressed_main_loop
  // End of main decompression loop
}
{
  // Slow-path of the needbits macro
  // Can be "called" from numerous places within pigz_available, though uses the
  // same stack frame as pigz_available, and the "return address" is in rax.
  |.align 16
  |->need_reader_bits:
  // Pre-conditions: Usual pigz_available stack frame and register assignments
  // Pre-conditions: rax contains the "return address" at the end a needbits macro
  // Pre-conditions: ecx, rather than Enbits, contains state->nbits
  // Pre-conditions: 0 <= ecx < 32
  // Pre-conditions: less than four input bytes remain
  // Clobbers: rcx, rdx, xmm0-15
  |  sub Rinend, Rinput
  |  add Einend, 4
  |4:
  // Pre-conditions: Einend contains the number of input bytes remaining
  // Pre-conditions: 0 <= Einend < 4
  // Read two bytes of input, if possible
  |  test Binend, 2
  |  jz >1
  |  movzx edx, word [Rinput]
  |  add Rinput, 2
  |  shl rdx, cl
  |  add ecx, 16
  |  sub Einend, 2
  |  or Rbits, rdx
  |1:
  // Pre-conditions: 0 <= Einend < 2
  // Read one byte of input, if possible
  |  test Binend, Binend
  |  jz >2
  |  movzx edx, byte [Rinput]
  |  shl rdx, cl
  |  add ecx, 8
  |  xor Einend, Einend
  |  or Rbits, rdx
  |2:
  // Pre-conditions: Einend == 0
  |  test cl, 32
  |  jnz >3
  // Call the reader function to get more input
  |  mov Stack->need_reader_bits_continuation, rax
  |  mov State->bits, Rbits
  |  mov State->nbits, cl
  |  mov State->crc, Ecrc
  |  mov Rarg1, State->opaque
  |  mov Rarg2, State
  |  call aword State->reader
  |  mov Rinend, [State]
  |  mov Rinput, rax
  |  mov rax, Stack->need_reader_bits_continuation
  |  mov Rbits, State->bits
  |  movzx ecx, byte State->nbits
  |  mov Ecrc, State->crc
  |  lea Rcrc_table, [->pigz_crc_table]
  |  test Rinend, Rinend
  |  jz ->unexpected_eof
  |  cmp Rinend, 4
  |  jb <4
  // Known: At least four bytes are available at Rinput
  |  sub rax, 16 // The instructions before rax will load 32 bits from Rinput into Rbits
  |3:
  |  lea Rinend, [Rinput + Rinend - 4]
  |  mov Enbits, ecx
  |  jmp rax
  |->unexpected_eof:
  // Pre-conditions: reader function returned a zero-length chunk
  |  cmp byte State->status, 12
  |  mov byte State->status, PIGZ_STATUS_UNEXPECTED_EOF ^ 0x80
  |  jb >1
  |  or rcx, Rwritepos
  |  jnz >1
  // If status was 12/13 ("expecting gzip header"), and writepos was zero, and
  // nbits was zero, then EOF was expected. Otherwise, it was unexpected.
  |  mov byte State->status, PIGZ_STATUS_EOF ^ 0x80
  |1:
  |  jmp ->return_from_available
  // End of need_reader_bits
}

|->fetch_next_block_bmi2:
|  add Rinend, 11
|->fetch_next_block:
// Pre-conditions: Usual pigz_available stack frame and register assignments
// Pre-conditions: A deflate block has just ended, and either another block
// or a gzip footer follows.
|  needbits
|  movzx ebx, byte State->status
|  test bl, 2
|  jnz ->gz_tail
|->fetch_next_block_got_bits:
// Pre-conditions: A deflate block is expected.
// Pre-conditions: 32 <= Enbits < 64
|  mov ebx, Ebits
|  and ebx, 7
|  shr Rbits, 3
|  sub Enbits, 3
|  cmp bl, 6
|  jae ->bad_bits // Invalid block type
|  shl ebx, 1
|  and byte State->status, 1
|  or State->status, bl
|  cmp bl, 4
|  jb ->prepare_uncompressed_block
|  cmp bl, 8
|  jb ->prepare_static_huffman_block
{
  // Known: BTYPE==10 (the next block is "compressed with dynamic Huffman codes")
  // Read the dynamic Huffman codes, and prepare for a compressed block... (all
  // the code between here and "->status_dispatch" is dedicated to this task)
  |  mov ebx, Ebits
  |  shr Rbits, 10
  |  mov eax, ebx
  |  shr ebx, 5
  |  and ebx, 31
  |  and eax, 31
  |  cmp bl, 30
  |  jae ->bad_bits // Too many distance codes
  |  mov bh, al
  |  cmp bh, 30
  |  jae ->bad_bits // Too many literal codes
  |  add ebx, 0x00010101
  // Known: ebx contains (num-literal-codes << 8) | (num-distance-codes)
  // (so bl is num-distance-codes, and bh is num-literal-codes minus 256)
  |  mov r12d, Ebits
  |  shr Rbits, 4
  |  and r12d, 15
  // Known: r12d contains num-code-length-codes minus 4
  // Accumulate code lengths for the code length alphabet into r13 (treating r13 as 19x3 bits)
  |  mov r13d, Ebits
  |  shr Rbits, 9
  |  and r13d, 0x1ff
  |  mov eax, Ebits
  |  shr Rbits, 3
  |  shl r13, 48
  |  and eax, 7
  |  or r13, rax
  |  sub Enbits, 26
  {
    |  test r12b, r12b
    |  jz ->got_ccodelengths
    |  mov64 r15, 0xf1e2d3c4b5a6978 // The order in which code lengths for the code length alphabet are given (after the first four)
    |->next_ccodelength:
    // Invariant: r12d contains number of code lengths for the code length alphabet still to read
    // Invariant: (r15 & 15) gives the index of the next code length
    |  needbits
    |  mov ecx, r15d
    |  mov eax, Ebits
    |  shr Rbits, 3
    |  and ecx, 15
    |  and eax, 7
    |  lea ecx, [ecx+ecx*2]
    |  sub Enbits, 3
    |  shl rax, cl
    |  shr r15, 4
    |  or r13, rax
    |  sub r12d, 1
    |  jnz ->next_ccodelength
    |->got_ccodelengths:
  }
  // Known: r13 contains code lengths for the code length alphabet
  {
    // Accumulate into r15 the number of code length alphabet entries per code
    // length (treating r15 as 8x8 bits), and also put this into xmm0.
    |  xor r15d, r15d
    |  lea rcx, [r13*8]
    |  pxor xmm1, xmm1
    |->next_npercodelength:
    |  ror r15, cl // NB: Ignores the high two bits of cl
    |  add r15, 1
    |  rol r15, cl // NB: Ignores the high two bits of cl
    |  shr rcx, 3
    |  and rcx, -8
    |  jnz ->next_npercodelength
    |  movd xmm0, r15
  }
  // Known: xmm1 == 0
  // Known: ecx == 0
  {
    // Turn r15 from sum into cumulative sum, and check that the code length
    // alphabet has a valid Huffman table.
    |  shr r15, 8
    |  lea eax, [ecx+2]
    |  lea r12d, [ecx+6]
    |  movzx edx, r15b
    |  sub eax, edx
    |  jl ->bad_bits
    |->next_code_npercodelength:
    |  ror r15, 8
    |  movzx ecx, r15b
    |  add eax, eax
    |  add r15, rdx
    |  add edx, ecx
    |  sub eax, ecx
    |  jl ->bad_bits // Code length alphabet over-subscribed.
    |  sub r12d, 1
    |  jnz ->next_code_npercodelength
    |  punpcklbw xmm0, xmm1 // Turn xmm0 from 8x8 bits to 8x16 bits (zero extend each uint8_t to uint16_t)
    |  test eax, eax
    |  jnz ->bad_bits // Code length alphabet under-subscribed.
  }
  {
    // Create opcodes for code length alphabet
    |  mov eax, 0x01000000
    |  lea r12, Stack->sorted_dist
    |  movdqa Stack->nper_lit, xmm0
    {
      |->next_codetable:
      |  sub al, 1
      |.macro codetable_entry
      |  mov ecx, r13d
      |  shr r13, 3
      |  and ecx, 7
      |  mov ah, cl
      |  shl ecx, 3
      |  ror r15, cl
      |  movzx edx, r15b
      |  add r15, 1
      |  mov [r12 + edx*4], eax
      |  rol r15, cl
      |.endmacro
      |  codetable_entry
      |  cmp al, 240
      |  jnz ->next_codetable
    }
    |  mov eax, 0x03020000
    |  codetable_entry
    |  movd xmm0, ebx // Spill ebx
    |  mov eax, 0x030300ff
    |  codetable_entry
    |  movd xmm1, Ecrc // Spill Ecrc
    |  mov eax, 0x0b0700ff
    |  codetable_entry
    |  shr r15, 56
    |  lea r13, Stack->nper_lit
    |  mov [r12 + r15*4 + 1], al
  }
  {
    // Create 128-entry lookup table for code length alphabet
    |  mov ecx, 7
    |  add Rstate, offsetof(pigz_state, litcodes)
    |  call ->make_tables
    |  sub Rstate, offsetof(pigz_state, litcodes)
    |  movd ebx, xmm0 // Restore ebx
  }
  {
    // Set r15d to the number of code lengths to read
    |  movzx r15d, bl
    |  mov eax, ebx
    |  shr eax, 8
    |  movd Ecrc, xmm1 // Restore Ecrc
    |  add r15d, eax
  }
  {
    // Make sure that the first code length code does not rely on the current code length
    |  mov eax, Ebits
    |  and eax, 127
    |  lea Rcrc_table, [->pigz_crc_table]
    |  test byte [Rstate + eax*4 + offsetof(pigz_state, litcodes)], 128
    |  jz ->bad_bits
  }
  {
    // Use the code length alphabet lookup table to decode the array of code lengths.
    // Invariant: r12d = current code length (initially undefined)
    // Invariant: r13d = number of code lengths which have been decoded
    // Invariant: 0 <= r13d <= r15d
    |  xor r13d, r13d
    |->next_codelength:
    |  needbits
    |  mov eax, Ebits
    |  and eax, 127
    |  mov ecx, [Rstate + eax*4 + offsetof(pigz_state, litcodes)]
    |  movsx eax, cl
    |  shr ecx, 8
    |  mov edx, eax
    |  sar eax, 6
    |  or r12d, eax
    |  shr Rbits, cl
    |  sub r12d, edx
    |  xor edx, edx
    |  sub Enbits, ecx
    |  shr ecx, 8
    |  shrd edx, Ebits, cl
    |  shr Rbits, cl
    |  rol edx, cl
    |  sub Enbits, ecx
    |  add edx, r13d
    |  shr ecx, 8
    |  add edx, ecx
    |  and Enbits, 63
    |  cmp edx, r15d
    |  ja ->bad_bits // More code lengths specified than expected.
    {
      |->store_next_codelength:
      |  mov [Rstate + r13d + offsetof(AltState, codelengths)], r12b
      |  add r13d, 1
      |  cmp r13d, edx
      |  jnz ->store_next_codelength
    }
    |  cmp edx, r15d
    |  jnz ->next_codelength
  }
  {
    // Count the number of literal codes per length, and the number of distance
    // codes per length.
    // Reminder: ebx contains (num-literal-codes << 8) | (num-distance-codes)
    |  xorps xmm0, xmm0
    |  movaps [rsp + offsetof(Stack, nper_lit)], xmm0
    |  mov ecx, ebx
    |  movaps [rsp + offsetof(Stack, nper_lit) + 16], xmm0
    |  xor eax, eax
    |  cmp al, AltState->codelengths[256]
    |  jz ->bad_bits // End-of-block is not in the literal alphabet
    |  shr ecx, 8
    |  movaps [rsp + offsetof(Stack, nper_dist)], xmm0
    |  lea rdx, [Rstate + rcx + offsetof(AltState, codelengths)]
    |  movaps [rsp + offsetof(Stack, nper_dist) + 16], xmm0
    {
      |->next_both_nper:
      |  movzx r12d, byte [Rstate + rax + offsetof(AltState, codelengths)]
      |  movzx r13d, byte [rdx + rax]
      |  add eax, 1
      |  add word [rsp + r12*2 + offsetof(Stack, nper_lit)], 1
      |  add byte [rsp + r13*2 + offsetof(Stack, nper_dist)], 1
      |  cmp al, bl
      |  jnz ->next_both_nper
    }
    {
      |->next_one_nper:
      |  movzx r12d, byte [Rstate + rax + offsetof(AltState, codelengths)]
      |  add eax, 1
      |  add word [rsp + r12*2 + offsetof(Stack, nper_lit)], 1
      |  cmp eax, ecx
      |  jnz ->next_one_nper
    }
  }
  {
    // Use counts in nper_lit to create cumulative sums in offset_lit, and
    // check that the literal alphabet has a valid Huffman table, and set
    // edx to the longest literal code length.
    |  mov eax, 1
    |  mov ecx, eax
    |  xor edx, edx
    |  xor r12d, r12d
    {
      |->next_offset_lit:
      |  movzx r13d, word [rsp + rax*2 + offsetof(Stack, nper_lit)]
      |  mov [Rstate + rax*2 + offsetof(AltState, offset_lit)], r12w
      |  add ecx, ecx
      |  test r13d, r13d
      |  cmovnz edx, eax
      |  add r12d, r13d
      |  add eax, 1
      |  sub ecx, r13d
      |  jl ->bad_bits // Literal alphabet over-subscribed.
      |  cmp eax, 16
      |  jnz ->next_offset_lit
    }
    |  test ecx, ecx
    |  jz >1
    |  cmp edx, 1
    |  jnz ->bad_bits // Literal alphabet under-subscribed.
    |1:
  }
  {
    // Use counts in nper_dist to create cumulative sums in offset_dist, and
    // check that the distance alphabet has a valid Huffman table, and set
    // r15d to the longest distance code length.
    |  mov eax, 1
    |  mov ecx, eax
    |  xor r15d, r15d
    |  xor r12d, r12d
    {
      |->next_offset_dist:
      |  movzx r13d, byte [rsp + rax*2 + offsetof(Stack, nper_dist)]
      |  mov [Rstate + rax + offsetof(AltState, offset_dist)], r12b
      |  add ecx, ecx
      |  test r13d, r13d
      |  cmovnz r15d, eax
      |  add r12d, r13d
      |  add eax, 1
      |  sub ecx, r13d
      |  jl ->bad_bits // Distance alphabet over-subscribed.
      |  cmp eax, 16
      |  jnz ->next_offset_dist
    }
    |  test ecx, ecx
    |  jz >1
    |  cmp r15d, 1
    |  jnz ->bad_bits // Distance alphabet under-subscribed.
    |1:
  }
  {
    // Set state->litbits and state->distbits
    |  mov eax, 9
    |  lea ecx, [eax-3]
    |  cmp eax, edx
    |  cmova eax, edx
    |  cmp ecx, r15d
    |  cmova ecx, r15d
    |  mov State->litbits, al
    |  mov State->distbits, cl
  }
  {
    // Create opcodes for the literal part of the literal alphabet
    |  xor eax, eax
    |->next_sorted_lit:
    |  movzx r12d, byte [Rstate + rax + offsetof(AltState, codelengths)]
    |  test r12b, r12b
    |  jz >1
    |  mov edx, r12d
    |  mov dh, al
    |  shl edx, 8
    |  movzx ecx, word [Rstate + r12*2 + offsetof(AltState, offset_lit)]
    |  or edx, 64
    |  mov [Rstate + rcx*4 + offsetof(AltState, sorted_lit)], edx
    |  add ecx, 1
    |  mov [Rstate + r12*2 + offsetof(AltState, offset_lit)], cx
    |1:
    |  add al, 1
    |  jnz ->next_sorted_lit
  }
  {
    // Create opcodes for the non-literal part of the literal alphabet
    |->next_sorted_lit_op:
    |  movzx ecx, byte [Rstate + rax + offsetof(AltState, codelengths) + 256]
    |  test cl, cl
    |  jz >1
    |  mov edx, [Rcrc_table + rax*4 + OFFSET_lit_ops]
    |  movzx r12d, word [Rstate + rcx*2 + offsetof(AltState, offset_lit)]
    |  mov dh, cl
    |  mov [Rstate + r12*4 + offsetof(AltState, sorted_lit)], edx
    |  add r12d, 1
    |  mov [Rstate + rcx*2 + offsetof(AltState, offset_lit)], r12w
    |1:
    |  add al, 1
    |  cmp al, bh
    |  jnz ->next_sorted_lit_op
  }
  {
    // Create opcodes for the distance alphabet
    |  xor r13d, r13d
    |  lea rax, [Rstate + rax + offsetof(AltState, codelengths) + 256]
    |->next_dist_op:
    |  movzx ecx, byte [r13 + rax]
    |  test cl, cl
    |  jz >1
    |  mov edx, [Rcrc_table + r13*4 + OFFSET_dist_ops]
    |  movzx r12d, byte [Rstate + rcx + offsetof(AltState, offset_dist)]
    |  mov dh, cl
    |  mov [rsp + r12*4 + offsetof(Stack, sorted_dist)], edx
    |  add r12d, 1
    |  mov [Rstate + rcx + offsetof(AltState, offset_dist)], r12b
    |1:
    |  add r13b, 1
    |  cmp r13b, bl
    |  jnz ->next_dist_op
  }
  {
    // Put sentinel entries at the end of the opcode arrays
    |  movzx ecx, word AltState->offset_lit[15]
    |  xor eax, eax
    |  movzx edx, byte AltState->offset_dist[15]
    |  not eax
    |  mov [rbp + rcx*4 + offsetof(AltState, sorted_lit)], eax
    |  mov [rsp + rdx*4 + offsetof(Stack, sorted_dist)], eax
  }
  {
    // Create the lookup tables for the literal alphabet and the distance alphabet
    |  mov eax, 128 + (1 << 16)
    |  mov State->litcodes[0], eax
    |  movd xmm0, Ecrc // Spill Ecrc
    |  mov State->litcodes[1], eax
    |  mov State->distcodes[0], eax
    |  movd xmm2, Rinend // Spill Rinend
    |  mov State->distcodes[1], eax
    |  movd xmm3, Rinput // Spill Rinput
    |  lea r12, AltState->sorted_lit
    |  movzx ecx, byte State->litbits
    |  add Rstate, offsetof(pigz_state, litcodes)
    |  test cl, cl
    |  jz >1
    |  lea r13, Stack->nper_lit
    |  call ->make_tables
    |1:
    |  lea r12, Stack->sorted_dist
    |  movzx ecx, byte [Rstate + offsetof(pigz_state, distbits) - offsetof(pigz_state, litcodes)]
    |  sub Rstate, offsetof(pigz_state, litcodes) - offsetof(pigz_state, distcodes)
    |  test cl, cl
    |  jz >1
    |  lea r13, Stack->nper_dist
    |  call ->make_tables
    |1:
    |  movd Rinend, xmm2 // Restore Rinend
    |  movd Rinput, xmm3 // Restore Rinput
    |->make_tables_tidyup_state_dispatch:
    |  sub Rstate, offsetof(pigz_state, distcodes)
    |  movd Ecrc, xmm0 // Restore Ecrc
    |->load_Rcrc_table_status_dispatch:
    |  lea Rcrc_table, [->pigz_crc_table]
  }
}
|->status_dispatch:
|  movzx eax, byte State->status
|  mov Rwritegoal, State->readpos
|  add Rwritegoal, PIGZ_READ_SIZE
|  cmp eax, 4
|  jl ->fetch_uncompressed
|  cmp eax, 12
|  jge ->gz_head
|  movzx ecx, word State->litbits
|  test eax, 1
|  jz >1
|  sub Rinend, 11
|  mov Elitmask, ecx
|  cmp Rinend, Rinput
|  jb ->fetch_compressed_main_loop_bmi2
|  mov r13, [Rinput]
|  jmp ->fetch_compressed_main_loop_bmi2
|1:
|  movzx Elitmask, byte [Rcrc_table + 114] // 1
|  mov Edistmask, Elitmask
|  shl Elitmask, cl
|  shr ecx, 8
|  shl Edistmask, cl
|  sub Elitmask, 1
|  sub Edistmask, 1
|  jmp ->fetch_compressed_main_loop
|->gz_head:
|  cmp Rwritepos, State->readpos
|  jnz ->return_from_available
{
  |  xor Ewritepos, Ewritepos
  |  mov State->readpos, Rwritepos
  |  needbits
  |  xor Ecrc, Ecrc
  |  mov State->readpos, Rwritepos
  |  mov eax, Ebits
  |  not Ecrc
  |  and eax, 0xe0ffffff
  |  shld ebx, Ebits, 8
  |  cmp eax, 0x00088b1f
  |  jnz ->bad_gz_magic
  {
    |  mov r12d, 10
    |->read_more_header_bytes:
    |  needbits
    |  movzx eax, Wwritepos
    |  movzx ecx, Bbits
    |  add Rwritepos, 1
    |  shr Rbits, 8
    |  xor cl, Bcrc
    |  shr Ecrc, 8
    |  xor Ecrc, [Rcrc_table + rcx*4]
    |  sub Enbits, 8
    |  cmp Ewritepos, r12d
    |  jnz ->read_more_header_bytes
    |  test bl, 4
    |  jz >1
    |  xor bl, 4
    |  movzx eax, Wbits
    |  lea r12d, [r12d+eax+2]
    |  jmp ->read_more_header_bytes
    |1:
  }
  {
    |  test bl, 24
    |  jz >1
    |->read_next_strz:
    |  mov eax, 8
    |  test bl, al
    |  setz cl
    |  shl eax, cl
    |  xor bl, al
    {
      |->read_more_strz:
      |  needbits
      |  movzx ecx, Bbits
      |  mov eax, ecx
      |  shr Rbits, 8
      |  xor cl, Bcrc
      |  shr Ecrc, 8
      |  sub Enbits, 8
      |  xor Ecrc, [Rcrc_table + rcx*4]
      |  test al, al
      |  jnz ->read_more_strz
    }
    |  test bl, 24
    |  jnz ->read_next_strz
    |1:
  }
  |  test bl, 2
  |  jz >1
  |  needbits
  |  not Ecrc
  |  cmp Wcrc, Wbits
  |  jnz ->bad_crc
  |  shr Rbits, 16
  |  sub Enbits, 16
  |1:
  |  needbits
  |  xor Ecrc, Ecrc
  |  xor Ewritepos, Ewritepos
  |  not Ecrc
  |  jmp ->fetch_next_block_got_bits
}
|
|->bad_crc:
|  mov byte State->status, PIGZ_STATUS_BAD_CRC ^ 0x80
|1:
|  jmp ->return_from_available
|->bad_gz_magic:
|  mov byte State->status, PIGZ_STATUS_BAD_HEADER ^ 0x80
|  jmp <1
|
|->gz_tail:
|  mov ecx, Enbits
|  and ecx, 7
|  shr Rbits, cl
|  sub Enbits, ecx
|  not Ecrc
|  needbits
|.if CRC
|  xor Ecrc, Ebits
|  jnz ->bad_crc
|.endif
|  shr Rbits, 32
|  sub Enbits, 32
|  needbits
|  cmp Ewritepos, Ebits
|  jnz ->bad_bits
|  shr Rbits, 32
|  sub Enbits, 32
|  or byte State->status, 12
|  jmp ->status_dispatch

|->fetch_uncompressed:
|  movzx ebx, word State->litlen
|  sub Rwritegoal, Rwritepos
|  jbe ->return_from_available
|  test ebx, ebx
|  jz ->fetch_next_block
|  cmp ebx, Ewritegoal
|  cmova ebx, Ewritegoal
|  sub State->litlen, bx
{
  |->fetch_uncompressed_main_loop:
  |  needbits
  |  movzx eax, Bbits
  |  movzx ecx, Wwritepos
  |  shr Rbits, 8
  |  inc Rwritepos
  |  sub Enbits, 8
  |  mov [Rstate + ecx + offsetof(pigz_state, window)], al
  |.if CRC
  |  xor al, Bcrc
  |  shr Ecrc, 8
  |  xor Ecrc, [Rcrc_table + eax*4]
  |.endif
  |  sub ebx, 1
  |  jnz ->fetch_uncompressed_main_loop
}
|  cmp State->litlen, bx
|  jz ->fetch_next_block
|  jmp ->return_from_available

|->prepare_uncompressed_block:
|  mov ecx, Enbits
|  and ecx, 7
|  shr Rbits, cl
|  sub Enbits, ecx
|  movzx ebx, Wbits
|  shr Rbits, 16
|  sub Enbits, 16
|  mov State->litlen, bx
|  not ebx
|  needbits
|  cmp bx, Wbits
|  jnz ->bad_bits
|  shr Rbits, 16
|  sub Enbits, 16
|  jmp ->status_dispatch

|->prepare_static_huffman_block:
|  xor ecx, ecx
|  mov dword Stack->nper_lit[8], 152 + (112 << 16)
|  movd xmm0, Ecrc // Spill Ecrc
{
  |1:
  |  mov eax, [Rcrc_table + ecx*4 + OFFSET_lit_ops]
  |  mov [Rstate + ecx*4 + offsetof(AltState, sorted_lit)], eax
  |  inc ecx
  |  cmp ecx, 24
  |  jnz <1
}
|  mov dword Stack->nper_lit[6], 32 + (24 << 16)
{
  |1:
  |  mov eax, [Rcrc_table + ecx*4 + OFFSET_lit_ops]
  |  mov [Rstate + ecx*4 + 144*4 + offsetof(AltState, sorted_lit)], eax
  |  inc ecx
  |  cmp ecx, 32
  |  jnz <1
}
|  xor ecx, ecx
|  lea r13, Stack->nper_lit
|  mov byte State->litbits, 9
{
  |1:
  |  lea eax, [ecx + (64 + (8 << 8)) << 16]
  |  rol eax, 16
  |  mov [Rstate + ecx*4 + 24*4 + offsetof(AltState, sorted_lit)], eax
  |  inc ecx
  |  cmp ecx, 144
  |  jnz <1
}
|  lea r12, AltState->sorted_lit
|  mov byte State->distbits, 5
{
  |1:
  |  lea eax, [ecx + (64 + (9 << 8)) << 16]
  |  rol eax, 16
  |  mov [Rstate + ecx*4 + 32*4 + offsetof(AltState, sorted_lit)], eax
  |  add cl, 1
  |  jnz <1
}
|  dec ecx
|  mov AltState->sorted_lit[288], ecx
|  add ecx, 10
|  add Rstate, offsetof(pigz_state, litcodes)
|  call ->make_tables
|  mov cl, 5
|  add r13, 2
|  lea r12, [->dist_ops]
|  sub Rstate, offsetof(pigz_state, litcodes) - offsetof(pigz_state, distcodes)
|  call ->make_tables
|  jmp ->make_tables_tidyup_state_dispatch
{
  // Start of main decompression loop, when CPU has support for BMI2
  // NB: The loop entry point is ->fetch_compressed_main_loop_bmi2

  |.align 16
  |->need_reader_bits_bmi2:
  |  cmp Enbits, 48
  |  jge ->got_reader_bits_bmi2
  |  lea rcx, [Rinend + 15]
  |  cmp Rinput, rcx
  |  jz >1
  |2:
  |  movzx edx, byte [Rinput]
  |  add Rinput, 1
  |  shlx rdx, rdx, Rnbits
  |  add Enbits, 8
  |  or Rbits, rdx
  |  cmp Enbits, 48
  |  jge ->got_reader_bits_bmi2
  |  cmp Rinput, rcx
  |  jnz <2
  |1:
  // Call the reader function to get more input
  |  mov State->bits, Rbits
  |  mov State->nbits, Bnbits
  |  mov State->crc, Ecrc
  |  mov Rarg1, State->opaque
  |  mov Rarg2, State
  |  call aword State->reader
  |  mov Rinend, [State]
  |  mov Rinput, rax
  |  mov Rbits, State->bits
  |  movzx Enbits, byte State->nbits
  |  mov Ecrc, State->crc
  |  lea Rcrc_table, [->pigz_crc_table]
  |  cmp Rinend, 15
  |  lea Rinend, [Rinput + Rinend - 15]
  |  jb >3
  |  mov r13, [Rinput]
  |  jmp ->got_reader_bytes_bmi2
  |3:
  |  lea rcx, [Rinend + 15]
  |  cmp Rinput, rcx
  |  jnz <2
  |  mov byte State->status, PIGZ_STATUS_UNEXPECTED_EOF ^ 0x80
  |  jmp ->return_from_available_bmi2

  |.align 16
  |->lit_not_lit_bmi2:
  // Pre-conditions: KIND == 2 (i.e. either end-of-block or error)
  // Pre-conditions: ebx contains VAL
  |  test ebx, ebx
  |  jz ->fetch_next_block_bmi2
  |->bad_bits_bmi2:
  |  mov byte State->status, PIGZ_STATUS_BAD_BITS ^ 0x80
  |->return_from_available_bmi2:
  |  add Rinend, 11
  |  jmp ->return_from_available
  |->lit_not_length_bmi2:
  // Pre-conditions: flags set by "cmp al, 64"
  // Pre-conditions: 1 <= KIND <= 2
  // Pre-conditions: al contains (KIND << 6)
  // Pre-conditions: ebx contains VAL
  |  .byte 0x2E; jnz ->lit_not_lit_bmi2
  // Known: KIND == 1 (i.e. VAL is a literal value)
  |  movzx eax, Wwritepos
  |  add Rwritepos, 1
  |  .byte 0x40; mov [Rstate + eax*1 + offsetof(pigz_state, window)], bl
  |.if CRC
  |  xor bl, Bcrc
  |  .byte 0x40; shr Ecrc, 8
  |  xor Ecrc, [Rcrc_table + ebx*4]
  |.endif
  |->fetch_compressed_main_loop_bmi2:
  // Pre-conditions: Usual pigz_available stack frame and register assignments
  |  cmp Rwritepos, Rwritegoal
  |  jae ->return_from_available_bmi2
  |  cmp Rinend, Rinput
  |  jb ->need_reader_bits_bmi2
  |->got_reader_bytes_bmi2:
  |  shlx rax, r13, Rnbits
  |  or Rbits, rax
  |  lea eax, [Enbits - 63]
  |  or Enbits, 56
  |  neg eax
  |  shr eax, 3
  |  add Rinput, rax
  |  mov r13, [Rinput]
  |->got_reader_bits_bmi2:
  // Known: enough bits are available for any literal/length code and then any distance code
  |  bzhi ebx, Ebits, Elitmask
  {
    |->load_lit_code_bmi2:
    // Pre-conditions: ebx is an index into state->litcodes
    // Replaces ebx with the VAL from litcodes
    |  mov eax, [Rstate + ebx*4 + offsetof(pigz_state, litcodes)]
    // Known: al contains (KIND << 6) | NXBITS
    |  movzx ecx, ah
    // Known: ecx contains NBITS
    |  mov ebx, eax
    |  and ebx, 63
    // Known: ebx contains NXBITS
    |  shrx Rbits, Rbits, rcx
    |  sub Enbits, ecx
    |  rorx rcx, rax, 16
    |  bzhi ebx, Ebits, ebx
    |  add ebx, ecx
    // Known: ebx contains VAL
    |  cmp al, 192
    |  jae ->load_lit_code_bmi2
  }
  |  sub Enbits, eax
  |  shrx Rbits, Rbits, rax // NB: Ignores the high two bits of al
  |  and Enbits, 63
  |  cmp al, 64
  |  jae ->lit_not_length_bmi2
  // Known: KIND == 0 (i.e. VAL is a length value)
  |  rorx edx, Elitmask, 8
  |  bzhi edx, Ebits, edx
  {
    |->load_dist_code_bmi2:
    // Pre-conditions: ebx contains a length value
    // Pre-conditions: edx is an index into state->distcodes
    // Replaces edx with the VAL from distcodes
    |  mov eax, [Rstate + edx*4 + offsetof(pigz_state, distcodes)]
    // Known: al contains (KIND << 6) | NXBITS
    |  movzx ecx, ah
    // Known: ecx contains NBITS
    |  mov edx, eax
    |  and edx, 63
    // Known: edx contains NXBITS
    |  shrx Rbits, Rbits, rcx
    |  sub Enbits, ecx
    |  rorx rcx, rax, 16
    |  bzhi edx, Ebits, edx
    |  add edx, ecx
    // Known: edx contains VAL
    |  cmp al, 192
    |  jae ->load_dist_code_bmi2
  }
  |  test al, 192
  |9:
  |  .byte 0x2E; jnz ->bad_bits_bmi2
  // Known: KIND == 0 (i.e. VAL is a distance value)
  // Replace edx with (writepos - edx) & (PIGZ_WINDOW_SIZE - 1)
  |  neg rdx
  |  sub Bnbits, al
  |  shrx Rbits, Rbits, rax
  |  add rdx, Rwritepos
  |  .byte 0x2E; js <9 // Distance is greater than writepos
  |  movzx edx, dx
  |  movzx ecx, Wwritepos
  // Start of backref-copy loop
  // Invariants: ebx contains number of bytes remaining to copy
  // Invariants: ecx contains the window index of the next byte to write
  // Invariants: edx contains the window index of the next byte to read
  |  add Rwritepos, rbx
  |.if CRC
  |  movd xmm0, esi // Temporarily spill esi
  |.endif
  |  test bl, 3
  |  jz ->backref_copy4_bmi2
  {
    |->backref_copy_bmi2: // One-byte-at-a-time backref-copy loop (at most three iterations)
    |  .byte 0x40; movzx eax, byte [Rstate + edx*1 + offsetof(pigz_state, window)]
    |  inc dx
    |  mov [Rstate + ecx*1 + offsetof(pigz_state, window)], al
    |.if CRC
    |  xor al, Bcrc
    |  shr Ecrc, 8
    |.endif
    |  inc cx
    |.if CRC
    |  xor Ecrc, [Rcrc_table + eax*4]
    |.endif
    |  .byte 0x81, 0xEB, 0x01, 0x00, 0x00, 0x00 // sub ebx, dword 1
    |  .byte 0x2E; jz ->fetch_compressed_main_loop_bmi2
    |  .byte 0x40; test ebx, 3
    |  jnz ->backref_copy_bmi2
  }
  {
    |->backref_copy4_bmi2: // Four-bytes-at-a-time backref-copy loop (at most 64 iterations)
    // Pre-conditions: 0 < ebx <= 256, (ebx & 3) == 0
    // The input might overlap the output, so reads from the window and writes to
    // the window are still done one byte at a time, but the unrolling massively
    // helps the CRC calculation (and also reduces ebx manipulations).
    for (i = 3; i >= 0; --i) {
      |  movzx eax, byte [Rstate + edx*1 + offsetof(pigz_state, window)]
      |  inc dx
      |  mov [Rstate + ecx*1 + offsetof(pigz_state, window)], al
      |.if CRC
      if (i) {
        |  xor al, Bcrc
        |  shr Ecrc, 8
      } else {
        |  xor eax, Ecrc
      }
      |.endif
      |  inc cx
      |.if CRC
      if (i == 3) {
        |  mov esi, [Rcrc_table + eax*4 + 256*4*i]
      } else if (i) {
        |  xor esi, [Rcrc_table + eax*4 + 256*4*i]
      } else {
        |  xor esi, [Rcrc_table + eax*4]
        |  mov Ecrc, esi
      }
      |.endif
    }
    |  sub ebx, 4
    |  jnz ->backref_copy4_bmi2
  }
  |.if CRC
  |  movd esi, xmm0 // Restore esi (it was spilled before the loop)
  |.endif
  // End of backref-copy loop
  |  jmp ->fetch_compressed_main_loop_bmi2
  // End of main decompression loop
}
|=>asm_cfi_endproc():

{
  |.align 16
  |->make_tables:
  |.if not WIN
  |=>asm_cfi_startproc():
  |.endif
  |  // input: rbp=table, r12=sorted, ecx=rootbits, r13=nper
  |  // phase 1 clobbers: r15, r11, rdx, rdi, rbx, rax, r12
  |  // phase 2 clobbers: r9, r10
  |.define Ehuff, r15d
  |.define Rnext_table, r11
  |.define Enext_table, r11d
  |  xor Enext_table, Enext_table
  |  bts Enext_table, ecx
  |  movzx edx, byte [r12+1]
  |  lea Rnext_table, [rbp + Enext_table*4]
  |  xor Ehuff, Ehuff
  {
    |->next_huff:
    |  xor edi, edi
    |  bts edi, edx
    |  lea rbx, [rbp + Ehuff*4]
    |  mov eax, [r12]
    |  add r12, 4
    {
      |->next_pos:
      |  mov [rbx], eax
      |  lea rbx, [rbx + edi*4]
      |  cmp rbx, Rnext_table
      |  jb ->next_pos
    }
    |  dec edi
    |  xor edi, Ehuff
    |  xor eax, eax
    |  bsr edi, edi
    |  bts eax, edi
    |  lea edi, [eax-1]
    |  and Ehuff, edi
    |  add Ehuff, eax
    |  sub word [r13 + edx*2], 1
    |  jnz ->next_huff
    |  movzx edx, byte [r12+1]
    |  cmp dl, cl
    |  jbe ->next_huff
  }
  |  js ->make_tables_ret
  |  xor r10d, r10d
  |  shrd r10d, Ehuff, cl
  {
    |->next_level2:
    |  xor eax, eax
    |  bts eax, edx
    |  mov edi, edx
    |  shr eax, cl
    {
      |->next_left:
      |  sub ax, [r13 + rdi*2]
      |  jle >1
      |  add eax, eax
      |  add edi, 1
      |  jmp ->next_left
      |1:
    }
    |  sub edi, ecx
    |  rol r10d, cl
    |  xor eax, eax
    |  mov r9, Rnext_table
    |  bts eax, edi
    |  lea Rnext_table, [Rnext_table+rax*4]
    |  mov rax, r9
    |  sub rax, rbp
    |  shl eax, 14
    |  mov al, 128+64
    |  mov ah, cl
    |  or al, r7b
    |  mov [rbp + r10*4], eax
    |  ror r10d, cl
    {
      |->next_level2_huff:
      |  mov eax, [r12]
      |  add r12, 4
      |  xor edi, edi
      |  mov ebx, Ehuff
      |  sub ah, cl
      |  shr ebx, cl
      |  bts edi, edx
      |  lea rbx, [r9 + rbx*4]
      |  shr edi, cl
      {
        |->next_level2_pos:
        |  mov [rbx], eax
        |  lea rbx, [rbx + edi*4]
        |  cmp rbx, Rnext_table
        |  jb ->next_level2_pos
      }
      |  sub word [r13 + rdx*2], 1
      |  jnz >1
      |  movzx edx, byte [r12+1]
      |  test dl, dl
      |  jns >1
      |->make_tables_ret:
      |  ret
      |1:
      |  shl edi, cl
      |  dec edi
      |  xor edi, Ehuff
      |  xor eax, eax
      |  bsr edi, edi
      |  bts eax, edi
      |  lea edi, [eax-1]
      |  and Ehuff, edi
      |  xor edi, edi
      |  add Ehuff, eax
      |  shrd edi, Ehuff, cl
      |  cmp edi, r10d
      |  jz ->next_level2_huff
    }
    |  mov r10d, edi
    |  jmp ->next_level2
  }
  |.if not WIN
  |=>asm_cfi_endproc():
  |.endif
}

{
  |.align 16
  |=>asm_export("pigz_init"):
  |->pigz_init:
  |.if not WIN
  |=>asm_cfi_startproc():
  |.endif
  |  xor eax, eax
  |  mov [Rarg1], rax
  |  mov State:Rarg1->readpos, rax
  |  mov State:Rarg1->writepos, rax
  |  mov State:Rarg1->opaque, Rarg2
  |  mov State:Rarg1->reader, Rarg3
  |  mov State:Rarg1->bits, rax
  |  or al, 4
  |  mov State:Rarg1->input, rax
  |  or al, 3
  |.if WIN
  |  push Rarg1
  |.endif
  |  xor ecx, ecx
  |  push rbx
  |  cpuid
  |  movzx eax, bh
  |  pop rbx
  |.if WIN
  |  pop Rarg1
  |.endif
  |  and al, 1
  |  or al, 12
  |  mov State:Rarg1->status, eax
  |  ret
  |.if not WIN
  |=>asm_cfi_endproc():
  |.endif
}

|.align 64
|->pigz_crc_table:
{
  int n, k;
  uint32_t tab[256], tab2[256];
  for (n = 0; n < 256; n++) {
    int32_t c = n;
    for (k = 0; k < 8; k++) {
      c = (0xedb88320L & -(c & 1)) ^ (int32_t)((uint32_t)c >> 1);
    }
    tab[n] = tab2[n] = (uint32_t)c;
    |.dword c
  }
  for (i = 0; i < 3; i++) {
    for (n = 0; n < 256; n++) {
      tab2[n] = (tab2[n] >> 8) ^ tab[tab2[n] & 0xff];
      |.dword tab2[n]
    }
  }
}
|->lit_ops:
|.dword 128 + (7 << 8), 0 + (7 << 8) + (3 << 16), 0 + (7 << 8) + (4 << 16)
|.dword 0 + (7 << 8) + (5 << 16), 0 + (7 << 8) + (6 << 16)
|.dword 0 + (7 << 8) + (7 << 16), 0 + (7 << 8) + (8 << 16)
|.dword 0 + (7 << 8) + (9 << 16), 0 + (7 << 8) + (10 << 16)
|.dword 1 + (7 << 8) + (11 << 16), 1 + (7 << 8) + (13 << 16)
|.dword 1 + (7 << 8) + (15 << 16), 1 + (7 << 8) + (17 << 16)
|.dword 2 + (7 << 8) + (19 << 16), 2 + (7 << 8) + (23 << 16)
|.dword 2 + (7 << 8) + (27 << 16), 2 + (7 << 8) + (31 << 16)
|.dword 3 + (7 << 8) + (35 << 16), 3 + (7 << 8) + (43 << 16)
|.dword 3 + (7 << 8) + (51 << 16), 3 + (7 << 8) + (59 << 16)
|.dword 4 + (7 << 8) + (67 << 16), 4 + (7 << 8) + (83 << 16)
|.dword 4 + (7 << 8) + (99 << 16)
|.dword 4 + (8 << 8) + (115 << 16), 5 + (8 << 8) + (131 << 16)
|.dword 5 + (8 << 8) + (163 << 16), 5 + (8 << 8) + (195 << 16)
|.dword 5 + (8 << 8) + (227 << 16), 0 + (8 << 8) + (258 << 16)
|.dword 128 + (8 << 8) + (1 << 16), 128 + (8 << 8) + (1 << 16)
|
|->dist_ops:
|.dword 0 + (5 << 8) + (1 << 16), 0 + (5 << 8) + (2 << 16), 0 + (5 << 8) + (3 << 16)
|.dword 0 + (5 << 8) + (4 << 16), 1 + (5 << 8) + (5 << 16), 1 + (5 << 8) + (7 << 16)
|.dword 2 + (5 << 8) + (9 << 16), 2 + (5 << 8) + (13 << 16)
|.dword 3 + (5 << 8) + (17 << 16), 3 + (5 << 8) + (25 << 16)
|.dword 4 + (5 << 8) + (33 << 16), 4 + (5 << 8) + (49 << 16)
|.dword 5 + (5 << 8) + (65 << 16), 5 + (5 << 8) + (97 << 16)
|.dword 6 + (5 << 8) + (129 << 16), 6 + (5 << 8) + (193 << 16)
|.dword 7 + (5 << 8) + (257 << 16), 7 + (5 << 8) + (385 << 16)
|.dword 8 + (5 << 8) + (513 << 16), 8 + (5 << 8) + (769 << 16)
|.dword 9 + (5 << 8) + (1025 << 16), 9 + (5 << 8) + (1537 << 16)
|.dword 10 + (5 << 8) + (2049 << 16), 10 + (5 << 8) + (3073 << 16)
|.dword 11 + (5 << 8) + (4097 << 16), 11 + (5 << 8) + (6145 << 16)
|.dword 12 + (5 << 8) + (8193 << 16), 12 + (5 << 8) + (12289 << 16)
|.dword 13 + (5 << 8) + (16385 << 16), 13 + (5 << 8) + (24577 << 16)
|.dword 128 + (5 << 8) + (1 << 16), 128 + (5 << 8) + (1 << 16), ~0
}
