#pragma once
#include "pigz.h"
#ifdef __cplusplus
extern "C" {
#endif

#define PIGZ_READ_SIZE (PIGZ_WINDOW_SIZE - 260)

typedef struct pigz_functions {
  void (*init)(pigz_state*, void*, pigz_reader);
  uint64_t (*available)(pigz_state*);
  const char* (*consume)(pigz_state*, uint64_t);
} pigz_functions;

uint32_t run_all_pigz_test_cases(const pigz_functions* funcs);

#ifdef __cplusplus
}
#endif
