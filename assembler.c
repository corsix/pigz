#include "dasm_proto.h"
#include "dasm_x86.h"
#include "test.h"
#include "pigz_s.h"
#include <stdio.h>
#ifdef _WIN32
#include <Windows.h>
#else
#include <sys/mman.h>
#if !defined(MAP_ANONYMOUS) && defined(MAP_ANON)
#define MAP_ANONYMOUS MAP_ANON
#endif
#endif

#ifdef _WIN32
typedef enum fixup_fn_kind {
  win32_fixup_kind_export = 1,
  win32_fixup_kind_startproc = 2,
  win32_fixup_kind_endproc = 3,
  win32_fixup_kind_push = 0 << 8,
  win32_fixup_kind_save = 4 << 8,
  win32_fixup_kind_sub = 1 << 8,
} fixup_fn_kind;
#else
typedef void (*fixup_fn_kind)(FILE*, void*);
#endif

typedef struct fixup {
  int lbl;
  int addr;
  fixup_fn_kind fn;
  void* arg;
} fixup_t;

static int compar_fixup(const void* lhs, const void* rhs) {
  const fixup_t* lhsF = (const fixup_t*)lhs;
  const fixup_t* rhsF = (const fixup_t*)rhs;
  if (lhsF->addr != rhsF->addr) {
    return lhsF->addr - rhsF->addr;
  }
  return lhsF->lbl - rhsF->lbl;
}

fixup_t fixups[100];
unsigned nfixups = 0;

int alloc_fixup(fixup_fn_kind fn, void* arg) {
  int result = nfixups + 20;
  fixups[nfixups].lbl = result;
  fixups[nfixups].fn = fn;
  fixups[nfixups].arg = arg;
  ++nfixups;
  return result;
}

void asm_export_fixup(FILE* f, void* arg) {
  const char* name = (const char*)arg;
#ifdef __linux__
  fprintf(f, ".globl %s\n", name);
  fprintf(f, ".type %s, @function\n", name);
  fprintf(f, "%s:\n", name);
#else
  fprintf(f, ".globl _%s\n", name);
  fprintf(f, "_%s:\n", name);
#endif
}

int asm_export(const char* name) {
#ifdef _WIN32
  return alloc_fixup(win32_fixup_kind_export, (void*)name);
#else
  return alloc_fixup(asm_export_fixup, (void*)name);
#endif
}

#ifndef _WIN32
void asm_cfi_fixup(FILE* f, void* arg) {
  const char* str = (const char*)arg;
  fprintf(f, ".cfi_%s\n", str);
  free(arg);
}

int asm_cfi(const char* fmt, ...) {
  char* buf = malloc(strlen(fmt) + 20);
  va_list args;
  va_start(args, fmt);
  vsprintf(buf, fmt, args);
  va_end(args);
  return alloc_fixup(asm_cfi_fixup, (void*)buf);
}
#endif

int asm_cfi_startproc() {
#ifdef _WIN32
  return alloc_fixup(win32_fixup_kind_startproc, 0);
#else
  return asm_cfi("startproc");
#endif
}

int asm_cfi_endproc() {
#ifdef _WIN32
  return alloc_fixup(win32_fixup_kind_endproc, 0);
#else
  return asm_cfi("endproc");
#endif
}

static const uint8_t dwarf_reg_to_msvc_reg[16] = {
  0, 2, 1, 3, 6, 7, 5, 4, 8, 9, 10, 11, 12, 13, 14, 15
};

int asm_cfi_push(int offset, int reg) {
#ifdef _WIN32
  (void)offset;
  return alloc_fixup(win32_fixup_kind_push, (void*)dwarf_reg_to_msvc_reg[reg]);
#else
  return asm_cfi("def_cfa_offset %d\n.cfi_offset %d, -%d", offset, reg, offset);
#endif
}

#if _WIN32
int asm_cfi_save(int offset, int reg) {
  return alloc_fixup(win32_fixup_kind_save, (void*)(ptrdiff_t)(dwarf_reg_to_msvc_reg[reg] + offset * 2));
}
#endif

int asm_cfi_sub(int offset, int delta) {
#if _WIN32
  (void)offset;
  return alloc_fixup(win32_fixup_kind_sub, (void*)(ptrdiff_t)(delta * 2));
#else
  (void)delta;
  return asm_cfi("def_cfa_offset %d", offset);
#endif
}

void pigz_assemble(pigz_functions* result) {
  dasm_State* Dst;
  void* globs[glob__MAX];
  size_t sz, i;
#ifdef _WIN32
  unsigned nprocs = 0;
#endif
  unsigned n;
  void* mem;
  const char* bprefix;
  FILE* f;
  dasm_init(&Dst, DASM_MAXSECTION);
  dasm_setupglobal(&Dst, globs, glob__MAX);
  dasm_setup(&Dst, actions);
  dasm_growpc(&Dst, 120);
  pigz_emit_asm(&Dst);
  dasm_link(&Dst, &sz);
#ifdef _WIN32
  i = 0;
  for (n = 0; n < nfixups; ++n) {
    switch (fixups[n].fn) {
    case win32_fixup_kind_startproc:
      ++nprocs;
      i += 16;
      break;
    case win32_fixup_kind_endproc:
      i += (i & 2);
      break;
    case win32_fixup_kind_push:
      i += 2;
      break;
    case win32_fixup_kind_save:
      i += 4;
      break;
    case win32_fixup_kind_sub:
      i += 4;
      break;
    default:
      break;
    }
  }
  mem = VirtualAlloc(NULL, sz + i, MEM_RESERVE | MEM_COMMIT, PAGE_EXECUTE_READWRITE);
#else
  mem = mmap(0, sz, PROT_READ | PROT_WRITE | PROT_EXEC, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
#endif
  dasm_encode(&Dst, mem);
  result->init = globs[glob_pigz_init];
  result->available = globs[glob_pigz_available];
  result->consume =  pigz_consume;
  for (n = 0; n < nfixups; ++n) {
    fixups[n].addr = dasm_getpclabel(&Dst, fixups[n].lbl);
  }
  dasm_free(&Dst);
  qsort(fixups, nfixups, sizeof(fixup_t), compar_fixup);
#ifdef _WIN32
  {
    uint32_t* rtfuncs = (uint32_t*)((char*)mem + sz);
    uint16_t* unwinds = (uint16_t*)(rtfuncs + nprocs * 3);
    for (n = 0; n < nfixups; ++n) {
      switch (fixups[n].fn) {
      case win32_fixup_kind_startproc:
        rtfuncs[0] = (uint32_t)fixups[n].addr;
        rtfuncs[2] = (uint32_t)((char*)unwinds - (char*)mem);
        unwinds += 2;
        break;
      case win32_fixup_kind_save:
      case win32_fixup_kind_sub:
        *unwinds++ = (uint16_t)((uint32_t)(size_t)fixups[n].arg / 16);
        // fallthrough
      case win32_fixup_kind_push:
        *unwinds++ = (uint16_t)(((uint32_t)fixups[n].addr - rtfuncs[0]) + (uint32_t)fixups[n].fn + ((uint32_t)(size_t)fixups[n].arg << 12));
        break;
      case win32_fixup_kind_endproc: {
        uint16_t *a = (uint16_t*)((char*)mem + rtfuncs[2]), *b;
        a[0] = (uint16_t)(1 + (unwinds[-1] << 8));
        a[1] = (uint16_t)(unwinds - a - 2);
        b = unwinds - 1;
        if (a[1] & 1) {
          *unwinds++ = 0;
        }
        a += 2;
        for (; a < b; ++a, --b) {
          *a ^= *b;
          *b ^= *a;
          *a ^= *b;
        }
        rtfuncs[1] = (uint32_t)fixups[n].addr;
        rtfuncs += 3;
        break; }
      default:
        break;
      }
    }
    RtlAddFunctionTable((PRUNTIME_FUNCTION)((char*)mem + sz), nprocs, (DWORD64)mem);
  }
#endif
  n = 0;
  f = fopen("pigz_o.s", "w");
  fprintf(f, ".file \"pigz.s\"\n");
#ifdef __linux__
  fprintf(f, ".section .note.GNU-stack, \"\", @progbits\n");
#endif
  fprintf(f, ".text\n");
  fprintf(f, ".p2align 6");
  bprefix = ".byte";
  for (i = 0; i < sz; ++i) {
    if (n < nfixups && fixups[n].addr == (int)i) {
      fprintf(f, "\n");
      do {
#ifndef _WIN32
        fixups[n].fn(f, fixups[n].arg);
#endif
        ++n;
      } while (n < nfixups && fixups[n].addr == (int)i);
      bprefix = ".byte";
    } else if (!(i & 15)) {
      fprintf(f, "\n");
      bprefix = ".byte";
    }
    fprintf(f, "%s %d", bprefix, ((unsigned char*)mem)[i]);
    bprefix = ",";
  }
  fprintf(f, "\n.p2align 6\n");
  fclose(f);
}

int main() {
  pigz_functions asmf;
  pigz_assemble(&asmf);
  return run_all_pigz_test_cases(&asmf) != 0;
}
