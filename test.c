#include <stdint.h>
#include <memory.h>
#include <stdio.h>
#include "test.h"

typedef struct verbatim_test_case {
  const char* name;
  const char* input;
  const char* output;
  uint32_t input_len;
  int32_t output_len; // or error code if output is NULL
} verbatim_test_case_t;

static verbatim_test_case_t verbatim_test_cases[] = {
  {"no input", NULL, "", 0, 0},
  {"too short for magic", "\0", NULL, 1, PIGZ_STATUS_UNEXPECTED_EOF},
  {"wrong magic", "    ", NULL, 4, PIGZ_STATUS_BAD_HEADER},
  {"too short for header", "\x1f\x8b\x08\x00\x00\x00\x00\x00", NULL, 8, PIGZ_STATUS_UNEXPECTED_EOF},
  {"empty via litrl", "\x1f\x8b\x08\x00\x00\x00\x00\x00\x00\x00\xf9\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00", "", 23, 0},
  {"empty via fixed", "\x1f\x8b\x08\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00", "", 20, 0},
  {"empty via dnmic", "\x1f\x8b\x08\x00\x00\x00\x00\x00\x00\x00\x05\xc0\x81\x00\x00\x00\x00\x00\x90\xff\x6b\x00\x00\x00\x00\x00\x00\x00\x00\x00", "", 30, 0},
  {"double empty via fixed", "\x1f\x8b\x08\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00"
                             "\x1f\x8b\x08\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00", "", 40, 0},
  {"xyz via litrl", "\x1f\x8b\x08\x00\x00\x00\x00\x00\x00\x00\x01\x03\x00\xfc\xff\x78\x79\x7a\x67\xba\x8e\xeb\x03\x00\x00\x00", "xyz", 26, 3},
  {"bad litrl", "\x1f\x8b\x08\x00\x00\x00\x00\x00\x00\x00\x01\x03\x00\x03\x00\x78\x79\x7a\x67\xba\x8e\xeb\x03\x00\x00\x00", NULL, 26, PIGZ_STATUS_BAD_BITS},
  {"xyz via fixed", "\x1f\x8b\x08\x00\x00\x00\x00\x00\x00\x00\xab\xa8\xac\x02\xfc\x67\xba\x8e\xeb\x03\x00\x00\x00", "xyz", 23, 3},
  {"xyz bad crc", "\x1f\x8b\x08\x00\x00\x00\x00\x00\x00\x00\xab\xa8\xac\x02\xfc\x67\xba\x8e\xea\x03\x00\x00\x00", NULL, 23, PIGZ_STATUS_BAD_CRC},
  {"xyz bad length", "\x1f\x8b\x08\x00\x00\x00\x00\x00\x00\x00\xab\xa8\xac\x02\xfc\x67\xba\x8e\xeb\x04\x00\x00\x00", NULL, 23, PIGZ_STATUS_BAD_BITS},
  {"xyz via dnmic", "\x1f\x8b\x08\x00\x00\x00\x00\x00\x00\x00\x05\x83\x81\x00\x00\x00\x00\x40\xb6\x51\x0f\xb0\x01\x67\xba\x8e\xeb\x03\x00\x00\x00", "xyz", 31, 3},
  {"xyz three ways", "\x1f\x8b\x08\x00\x00\x00\x00\x00\x00\x00\x01\x03\x00\xfc\xff\x78\x79\x7a\x67\xba\x8e\xeb\x03\x00\x00\x00"
                     "\x1f\x8b\x08\x00\x00\x00\x00\x00\x00\x00\xab\xa8\xac\x02\xfc\x67\xba\x8e\xeb\x03\x00\x00\x00"
                     "\x1f\x8b\x08\x00\x00\x00\x00\x00\x00\x00\x05\x83\x81\x00\x00\x00\x00\x40\xb6\x51\x0f\xb0\x01\x67\xba\x8e\xeb\x03\x00\x00\x00", "xyzxyzxyz", 80, 9},
  {"incomplete dist code set", "\x1f\x8b\x08\x00\x00\x00\x00\x00\x00\x00\x05\x82\x81\x00\x00\x00\x00\x40\xb6\x51\x0f\x00\x00\x00\x00\x00", NULL, 26, PIGZ_STATUS_BAD_BITS},
  {"incomplete lit code set", "\x1f\x8b\x08\x00\x00\x00\x00\x00\x00\x00\x05\x83\x81\x00\x00\x00\x00\x40\xba\xa9\x07\x00\x00\x00\x00", NULL, 25, PIGZ_STATUS_BAD_BITS},
  {"overfull lit code set", "\x1f\x8b\x08\x00\x00\x00\x00\x00\x00\x00\x05\xc0\x81\x00\x00\x00\x00\x00\x90\x6d\xd4\x03\x00\x00\x00\x00", NULL, 26, PIGZ_STATUS_BAD_BITS},
  {"no EOB in lit code set", "\x1f\x8b\x08\x00\x00\x00\x00\x00\x00\x00\x05\xc0\x81\x00\x00\x00\x00\x00\x90\x6d\xf2\x01\x00\x00\x00\x00", NULL, 26, PIGZ_STATUS_BAD_BITS},
  {"hlit too large", "\x1f\x8b\x08\x00\x00\x00\x00\x00\x00\x00\xfd\x00\x00\x00\x00\x00", NULL, 16, PIGZ_STATUS_BAD_BITS},
  {"hdist too large", "\x1f\x8b\x08\x00\x00\x00\x00\x00\x00\x00\x05\x1f\x00\x00\x00\x00", NULL, 16, PIGZ_STATUS_BAD_BITS},
  {"incomplete code length set", "\x1f\x8b\x08\x00\x00\x00\x00\x00\x00\x00\xf5\xfe\xff\xff\xff\xff\xff\xff\xff\x03\x00\x00\x00\x00", NULL, 24, PIGZ_STATUS_BAD_BITS},
  {"overfull code length set", "\x1f\x8b\x08\x00\x00\x00\x00\x00\x00\x00\xf5\xfe\x49\x92\x24\x49\x92\x24\x49\x02\x00\x00\x00\x00", NULL, 24, PIGZ_STATUS_BAD_BITS},
  {"small text file", "\x1f\x8b\x08\x08\xee\x91\x66\x59\x04\x00\x78\x2e\x6c\x75\x61\x00\x55\x8d\xbd\x0a\x80\x20\x1c\xc4\xf7\xa0\x77\xb8\xd1\x3e\x06\x69\xce\xa7\x68\x0f\x4c\x2d\x04\xf9"
                      "\x1b\x69\x20\x3d\x7d\xd2\x50\xb6\x1c\xc7\xef\x8e\x3b\xe7\x95\x74\xd8\x0d\x69\x4b\x1b\x04\x78\x5d\xfd\xd0\x64\x2f\x53\xe2\xf5\x24\x15\xad\x27\x2c\x36\x06\x96\x7a"
                      "\x50\x53\x57\x80\x0c\xc1\x1c\x91\x71\x8c\x02\x09\x92\x74\xd6\x6c\x87\xf9\x89\xbf\xfd\xd7\x75\x60\x09\x6d\x2e\x14\x3f\xcd\x0d\x54\x03\x2a\x24\x8d\x00\x00\x00",
                      "local pending = 0\r\n"
                      "local pendingSize = 0\r\n"
                      "local function bits(x, n)\r\n"
                      "  assert(0 <= x and x <= 2^n)\r\n"
                      "  pending = pending + (x * 2^pendingSize)", 119, 141},
  {NULL, NULL, NULL, 0, 0},
};

typedef struct verbatim_reader {
  const char* input;
  uint32_t input_len;
  uint32_t chunk_size;
  char buf[4];
} verbatim_reader_t;

static const char* verbatim_reader_fn(void* opaque, uint64_t* len) {
  verbatim_reader_t* self = (verbatim_reader_t*)opaque;
  uint32_t chunk_len = self->input_len < self->chunk_size ? self->input_len : self->chunk_size;
  *len = chunk_len;
  if (chunk_len == 0) {
    return opaque;
  } else if (chunk_len <= 4) {
    memcpy(self->buf, self->input, chunk_len);
    self->input += chunk_len;
    self->input_len -= chunk_len;
    return self->buf;
  } else {
    const char* result = self->input;
    self->input += chunk_len;
    self->input_len -= chunk_len;
    return result;
  }
}

static uint32_t run_verbatim_test_cases(const pigz_functions* funcs) {
  uint32_t nfail = 0;
  pigz_state state;
  verbatim_test_case_t* test_case = verbatim_test_cases;
  verbatim_reader_t reader;
  for (; test_case->name; ++test_case) {
    for (reader.chunk_size = 1; reader.chunk_size <= (test_case->input_len ? test_case->input_len : 1); ++reader.chunk_size) {
      uint64_t available;
      uint64_t total = 0;
      reader.input = test_case->input;
      reader.input_len = test_case->input_len;
      printf("%s %d: ", test_case->name, (int)reader.chunk_size);
      funcs->init(&state, &reader, verbatim_reader_fn);
      while ((available = funcs->available(&state))) {
        const char* buf = funcs->consume(&state, available);
        if (test_case->output && memcmp(buf, test_case->output + total, available) != 0) {
          printf("FAIL (output mismatches)\n");
          ++nfail;
          goto next_test;
        }
        total += available;
      }
      if (state.status != (test_case->output ? PIGZ_STATUS_EOF : test_case->output_len)) {
        printf("FAIL (finished in state %d)\n", (int)state.status);
        ++nfail;
        goto next_test;
      }
      if (test_case->output && total != (uint32_t)test_case->output_len) {
        printf("FAIL (only produced %d of %d bytes)\n", (int)total, (int)test_case->output_len);
        ++nfail;
        goto next_test;
      }
      printf("PASS\n");
      next_test:;
    }
  }
  return nfail;
}

typedef struct one_byte_reader {
  int32_t crc;
  char byteval;
  uint8_t state;
  char buf[4];
} one_byte_reader_t;

static const char* one_byte_reader_literal(void* opaque, uint64_t* len) {
  one_byte_reader_t* self = (one_byte_reader_t*)opaque;
  switch (self->state++) {
  case 0: *len = 15; return "\x1f\x8b\x08\x00\x00\x00\x00\x00\x00\x00\x01\x01\x00\xfe\xff";
  case 1: *len = 1; return &self->byteval;
  case 3: self->crc = 1;
  case 2: *len = 4; return (const char*)&self->crc;
  case 4: *len = 0; return NULL;
  default: *len = 1; return "";
  }
}

static uint32_t revbits(uint32_t x, uint32_t n) {
  uint32_t result = 0;
  while (n--) {
    result = (result << 1) | (x & 1);
    x >>= 1;
  }
  return result;
}

static const char* one_byte_reader_static(void* opaque, uint64_t* len) {
  one_byte_reader_t* self = (one_byte_reader_t*)opaque;
  switch (self->state++) {
  case 0: *len = 10; return "\x1f\x8b\x08\x00\x00\x00\x00\x00\x00\x00";
  case 1: {
    uint32_t bits = 3;
    if ((uint8_t)self->byteval < 144) {
      bits |= (revbits(0x30 + (uint8_t)self->byteval, 8) << 3);
    } else {
      bits |= (revbits(0x100 + (uint8_t)self->byteval, 9) << 3);
    }
    memcpy(self->buf, &bits, 4);
    *len = 3;
    return self->buf;
  }
  case 3: self->crc = 1;
  case 2: *len = 4; return (const char*)&self->crc;
  case 4: *len = 0; return NULL;
  default: *len = 1; return "";
  }
}

static uint32_t run_one_byte_test_cases(const pigz_functions* funcs) {
  uint32_t nfail = 0;
  pigz_state state;
  one_byte_reader_t reader;
  const char* result;
  int n, k;
  for (n = 0; n < 256; n++) {
    int32_t c = n;
    for (k = 0; k < 8; k++) {
      c = (0xedb88320L & -(c & 1)) ^ (int32_t)((uint32_t)c >> 1);
    }
    reader.crc = c ^ 0xff000000;
    reader.byteval = (char)(n ^ 255);
    reader.state = 0;
    printf("one_byte_lit-%d: ", n);
    funcs->init(&state, &reader, one_byte_reader_literal);
    result = funcs->available(&state) ? funcs->consume(&state, 1) : NULL;
    if (!result || *result != reader.byteval || funcs->available(&state) || state.status != PIGZ_STATUS_EOF) {
      printf("FAIL\n");
      ++nfail;
    } else {
      printf("PASS\n");
    }
    reader.crc = c ^ 0xff000000;
    reader.byteval = (char)(n ^ 255);
    reader.state = 0;
    printf("one_byte_static-%d: ", n);
    funcs->init(&state, &reader, one_byte_reader_static);
    result = funcs->available(&state) ? funcs->consume(&state, 1) : NULL;
    if (!result || *result != reader.byteval || funcs->available(&state) || state.status != PIGZ_STATUS_EOF) {
      printf("FAIL\n");
      ++nfail;
    } else {
      printf("PASS\n");
    }
  }
  return nfail;
}

typedef struct fixed_size_record_reader {
  uint32_t length_produced;
  char initial[5 + 21];
  char record[5 + 21];
  uint8_t state;
  uint32_t crc;
  uint32_t crc_table[256];
} fixed_size_record_reader_t;

static const char* fixed_size_record_reader_fn(void* opaque, uint64_t* len) {
  fixed_size_record_reader_t* self = (fixed_size_record_reader_t*)opaque;
  if (self->state == 0) {
    *len = 10;
    self->state = 1;
    return "\x1f\x8b\x08\x00\x00\x00\x00\x00\x00\x00";
  }
  if (self->state == 1) {
    if (self->length_produced > 130000) {
      self->state = 2;
    } else {
      const char* result = self->length_produced ? self->record : self->initial;
      uint32_t crc = ~self->crc;
      uint32_t i;
      *len = 5 + *(uint16_t*)(result + 1);
      for (i = 5; i < *len; ++i) {
        crc = self->crc_table[(crc ^ result[i]) & 0xff] ^ (crc >> 8);
      }
      self->crc = ~crc;
      self->length_produced += *(uint16_t*)(result + 1);
      return result;
    }
  }
  if (self->state == 2) {
    *len = 5;
    self->state = 3;
    return "\x01\x00\x00\xff\xff";
  }
  if (self->state == 3) {
    *len = 4;
    self->state = 4;
    return (const char*)&self->crc;
  }
  if (self->state == 4) {
    *len = 4;
    self->state = 5;
    return (const char*)&self->length_produced;
  }
  *len = 0;
  return NULL;
}

static uint64_t readinto(pigz_state* state, const pigz_functions* funcs, char* buf, uint64_t len) {
  uint64_t avail;
  do {
    avail = funcs->available(state);
    if (avail > len) {
      avail = len;
    }
    memcpy(buf, funcs->consume(state, avail), avail);
    buf += avail;
    len -= avail;
  } while (avail);
  return len;
}

static uint32_t run_fixed_size_record_test_cases(const pigz_functions* funcs) {
  uint32_t i, j, k;
  uint32_t nfail = 0;
  pigz_state state;
  fixed_size_record_reader_t reader;
  const char* result;
  char buf[21];
  for (i = 0; i < 256; i++) {
    int32_t c = (int32_t)i;
    for (j = 0; j < 8; j++) {
      c = (0xedb88320L & -(c & 1)) ^ (int32_t)((uint32_t)c >> 1);
    }
    reader.crc_table[i] = (uint32_t)c;
  }
  reader.initial[0] = 0;
  reader.record[0] = 0;
  for (i = 1; i <= 21; ++i) {
    *(uint16_t*)(reader.initial + 1) = i;
    *(uint16_t*)(reader.initial + 3) = ~i;
    for (j = 1; j <= 21; ++j) {
      reader.length_produced = 0;
      reader.state = 0;
      reader.crc = 0;
      *(uint16_t*)(reader.record + 1) = j;
      *(uint16_t*)(reader.record + 3) = ~j;
      for (k = 0; k < i || k < j; ++k) {
        reader.initial[5 + k] = ~k;
        reader.record[5 + k] = 1 + k;
      }
      printf("fixed_size_record-%d-%d: ", (int)i, (int)j);
      funcs->init(&state, &reader, fixed_size_record_reader_fn);
      result = funcs->available(&state) >= i ? funcs->consume(&state, i) : NULL;
      if (!result || memcmp(result, reader.initial + 5, i)) {
        printf("FAIL\n");
        ++nfail;
        goto next_test;
      }
      for (;;) {
        memset(buf, 'a', sizeof(buf));
        result = readinto(&state, funcs, buf, j) ? NULL : buf;
        if (result == NULL && state.status == PIGZ_STATUS_EOF && reader.length_produced > 130000) {
          printf("PASS\n");
          goto next_test;
        }
        if (!result || memcmp(result, reader.record + 5, j)) {
          printf("FAIL\n");
          ++nfail;
          goto next_test;
        }
      }
      next_test:;
    }
  }
  return nfail;
}

uint32_t run_all_pigz_test_cases(const pigz_functions* funcs) {
  return run_verbatim_test_cases(funcs)
       + run_one_byte_test_cases(funcs)
       + run_fixed_size_record_test_cases(funcs)
       ;
}
