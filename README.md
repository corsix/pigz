# pigz - performant inflater for gzip files

pigz is a library for decompressing (inflating) gzipped data. It is written in x86-64 assembly, and intended for use by C/C++ programs. It is generally faster than zlib, however unlike zlib:
  * pigz is not portable (only x86-64)
  * pigz does not do any kind of compression (only decompresssion)
  * pigz cannot operate on raw deflate streams (only gzip streams)
  * pigz is not API compatible with zlib

## The API

The API is fully described in [pigz.h](https://github.com/corsix/pigz/blob/master/pigz.h), but the quick synopsis is:
```c
typedef struct pigz_state {
  ...
  int8_t status;
  ...
} pigz_state;

typedef const char* (*pigz_reader)(void* opaque, uint64_t* len);
void pigz_init(pigz_state* state, void* opaque, pigz_reader reader);

uint64_t pigz_available(pigz_state* state);

const char* pigz_consume(pigz_state* state, uint64_t len);

// Error values for pigz_state::status
#define PIGZ_STATUS_BAD_BITS -5
#define PIGZ_STATUS_BAD_CRC -4
#define PIGZ_STATUS_BAD_HEADER -3
#define PIGZ_STATUS_UNEXPECTED_EOF -2
#define PIGZ_STATUS_EOF -1
```
To begin, call `pigz_init`, passing a callback which will provide a gzip stream. Then call `pigz_available` and `pigz_consume` in a loop until `pigz_available` returns zero. Finally, check the `status` field to determine why decompression stopped. 

A complete example is provided in [ungz.c](https://github.com/corsix/pigz/blob/master/ungz.c):
```c
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
```

## Building / using pigz

The following commands will download pigz and build `pigz.o`. To use pigz in your project, include `pigz.h` and link against `pigz.o`.
```
git clone https://github.com/corsix/pigz
cd pigz
git submodule update --init
make pigz.o
```

## The code

If reading gnarly x86-64 assembly code is your thing, look at [pigz.s](https://github.com/corsix/pigz/blob/master/pigz.s). The syntax is that of [DynASM](https://corsix.github.io/dynasm-doc/index.html).
