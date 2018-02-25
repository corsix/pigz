#include "pigz.h"
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

static const char* read_from_stdin(void* opaque, uint64_t* len) {
  char* inbuf = (char*)opaque;
  for (;;) {
    ssize_t m = read(STDIN_FILENO, inbuf, 8192);
    if (m >= 0) {
      *len = (uint64_t)m;
      return inbuf;
    } else if (errno == EINTR || errno == EAGAIN) {
      continue;
    } else {
      fprintf(stderr, "Error %d reading from stdin\n", errno);
      exit(1);
    }
  }
}

int main() {
  pigz_state s;
  char inbuf[8192];
  uint64_t n;
  pigz_init(&s, inbuf, read_from_stdin);
  while ((n = pigz_available(&s))) {
    const char* buf = pigz_consume(&s, n);
    do {
      ssize_t m = write(STDOUT_FILENO, buf, n);
      if (m <= 0) {
        if (errno == EINTR || errno == EAGAIN) {
          continue;
        }
        fprintf(stderr, "Error %d writing to stdout\n", errno);
        return 1;
      }
      buf += m;
      n -= m;
    } while (n);
  }
  if (s.status != PIGZ_STATUS_EOF) {
    fprintf(stderr, "Error %d inflating gzip stream\n", s.status);
  }
  return 0;
}
