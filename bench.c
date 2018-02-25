#include "pigz.h"
#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>

typedef struct inputbuf {
  char* buf;
  uint32_t len;
  uint32_t n;
} inputbuf_t;

static const char* readbuf(void* opaque, uint64_t* len) {
  inputbuf_t* self = (inputbuf_t*)opaque;
  if (self->n) {
    --self->n;
    *len = self->len;
    return self->buf;
  } else {
    *len = 0;
    return NULL;
  }
}

int main(int argc, const char** argv) {
  const char* filename = (argc >= 2) ? argv[1] : "bench.gz";
  int fd = open(filename, O_RDONLY | O_CLOEXEC);
  struct stat st;
  inputbuf_t buf;
  pigz_state p;
  uint64_t n;
  if (fd < 0) {
    fprintf(stderr, "Cannot open input (%s): %s\n", filename, strerror(errno));
    return 1;
  }
  if (fstat(fd, &st) != 0) {
    fprintf(stderr, "Cannot stat input (%s): %s\n", filename, strerror(errno));
    return 1;
  }
  buf.buf = malloc(st.st_size);
  for (buf.len = 0; buf.len < st.st_size; ) {
    ssize_t m = read(fd, buf.buf + buf.len, st.st_size - buf.len);
    if (m <= 0) {
      if (errno == EINTR || errno == EAGAIN) {
        continue;
      }
      fprintf(stderr, "Cannot read input\n");
      return 1;
    }
    buf.len += m;
  }
  buf.n = (argc >= 3) ? atoi(argv[2]) : 2;
  pigz_init(&p, &buf, readbuf);
  while ((n = pigz_available(&p))) {
    pigz_consume(&p, n);
  }
  if (p.status != PIGZ_STATUS_EOF) {
    fprintf(stderr, "Error %d inflating gzip stream\n", p.status);
    return 1;
  }
  return 0;
}
