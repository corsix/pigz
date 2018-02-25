#pragma once
#include <stdint.h>
#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief Callback function used to provide chunks of gzipped input.
 *
 * Whenever @c pigz_available needs to fetch some more gzipped input in order
 * to make more uncompressed bytes available, it will call this callback. If
 * the end of input has been reached, zero should be stored to <tt>*len</tt>.
 * Otherwise, if the end of input has not been reached, at least one byte of
 * gzipped input must be provided. Once the callback has indicated that the
 * end of input has been reached for a given state, the callback will not be
 * called again (even if @c pigz_available is called again).
 *
 * @param opaque An opaque value, which is provided to @c pigz_init, and
 *               is passed through verbatim to the callback.
 * @param[out] len A pointer into which the callback should store the number
 *                 of bytes of gzipped input which it has made available. If
 *                 the end of input has been reached, zero should be stored.
 * @return A pointer to the start of the chunk of input. This pointer (and the
 *         <tt>*len</tt> bytes it refers to) must remain valid until the next
 *         time the callback is called for the same state. If the end of input
 *         has been reached, any value can be returned.
 */
typedef const char* (*pigz_reader)(void* opaque, uint64_t* len);

/**
 * @brief The size of the circular buffer of uncompressed bytes in a @c pigz_state.
 *
 * This value is provided for documentation only. It cannot be changed.
 */
#define PIGZ_WINDOW_SIZE 65536

/**
 * Value for @c pigz_state::status indicating that the input bitstream was
 * invalid.
 *
 * This value can arise from many different problems with the input, for
 * example: <ul>
 *  <li>A block with a @c BTYPE value of 11.</li>
 *  <li>An uncompressed block whose @c LEN and @c NLEN fields are not
 *      inverses of each other.</li>
 *  <li>A gzip footer whose @c ISIZE field mismatches the number of uncompressed
 *      bytes produced from the gzip file.</li>
 *  <li>A backref with distance greater than the number of uncompressed bytes
 *      produced so far.</li>
 *  <li>A dynamic Huffman table having too few entries (or, equivalently, having
 *      scope to reduce the lengths of some entries in the table).</li>
 *  <li>A dynamic Huffman table having too many entries (for example, having
 *      more than <tt>pow(2, N)</tt> entries of length @c N).</li>
 * </ul>
 */
#define PIGZ_STATUS_BAD_BITS -5

/**
 * Value for @c pigz_state::status indicating that either a gzip header CRC
 * was incorrect, or a gzip footer CRC was incorrect.
 */
#define PIGZ_STATUS_BAD_CRC -4

/**
 * Value for @c pigz_state::status indicating that a gzip file did not start
 * with the three-byte sequence 0x1F 0x8B 0x08, or a reserved bit was set in
 * the gzip header's @c FLAGS field.
 */
#define PIGZ_STATUS_BAD_HEADER -3

/**
 * Value for @c pigz_state::status indicating that the end of input was reached,
 * but that this did not happen at a gzip file boundary (i.e. the input was - at
 * best - a truncated gzip file, rather than a complete gzip file).
 */
#define PIGZ_STATUS_UNEXPECTED_EOF -2

/**
 * Value for @c pigz_state::status indicating that the end of input was reached,
 * and that this coincided with the end of a gzip file (i.e. the input was a
 * valid gzip file).
 *
 * This is the only negative value for @c pigz_state::status which doesn't
 * indicate a problem with the input.
 */
#define PIGZ_STATUS_EOF -1

/**
 * @brief Structure containing all the state required for uncompressing a gzip stream.
 *
 * All fields should be considered opaque, with the exception of @c status, which
 * can be inspected at any time to determine whether the end of uncompressed data
 * has been reached or whether an error has occured (see @c pigz_available). Other
 * fields are documented merely for the purpose of understanding the library's
 * implementation - users of the library should neither read nor write these fields.
 *
 * @c pigz_init should be used to initialise instances of this structure.
 */
typedef struct pigz_state {
  /**
   * A pointer to four bytes before the end of the current input chunk.
   */
  const char* inend;

  /**
   * A pointer to the next byte of the current input chunk. Conceptually,
   * a byte of input will be read from this pointer, those eight bits will be
   * absorbed into @c bits, and this pointer will be advanced by one (in
   * practice, this is typically done four bytes at a time, rather than one
   * byte at a time).
   */
  const char* input;

  /**
   * The total number of uncompressed bytes which @c pigz_consume has consumed
   * from the current gzip file. This value will not exceed @c writepos.
   */
  uint64_t readpos;

  /**
   * The total number of uncompressed bytes which @c pigz_available has made
   * available (i.e. has uncompressed) from the current gzip file. If this is
   * equal to @c readpos when @c pigz_available is called, more input will get
   * uncompressed. This value will not exceed <tt>readpos + PIGZ_WINDOW_SIZE - 1</tt>.
   */
  uint64_t writepos;

  /**
   * An opaque value which will be passed to @c reader.
   */
  void* opaque;

  /**
   * Callback function which will be used to obtain the next chunk of input.
   *
   * @see pigz_reader
   */
  pigz_reader reader;

  /**
   * To users of the library, an extra return value from @c pigz_available: if
   * @c pigz_available returns a non-zero value, then this field will be
   * non-negative. If @c pigz_available returns zero, then this field will be
   * negative, and will indicate why @c pigz_available returned zero (by being
   * one of @c PIGZ_STATUS_BAD_BITS, @c, PIGZ_STATUS_BAD_CRC,
   * @c PIGZ_STATUS_BAD_HEADER, @c, PIGZ_STATUS_UNEXPECTED_EOF, or
   * @c PIGZ_STATUS_EOF).
   *
   * Internal to the library, there are three kinds of non-negative value in
   * this field: <ul>
   *  <li>Values in the range 0 through 5 are used to denote the current block
   *      type and whether the current block is the final block (the low bit is
   *      @c BFINAL, the next two bits are @c BTYPE).</li>
   *  <li>The value 6 denotes being at a gzip file boundary - the @c CRC32 and
   *      @c ISIZE fields of the previous gzip file have been verified, but the
   *      header of the next gzip file has yet to be looked at.</li>
   *  <li>Values in the range 64 through 127 denote that an error has occurred,
   *      but some uncompressed bytes have yet to be consumed, so said error
   *      has not yet been revealed. Once the bytes have been consumed, it'll
   *      be revealed by toggling the sign bit.</li>
   * </ul>
   */
  int8_t status;

  /**
   * The number of bits which have been read from the input, but not yet
   * been used. Always in the range 0 through 63.
   */
  uint8_t nbits;

  union {
    /**
     * When uncompressing an uncompressed-block, the number of literal bytes
     * remaining in the block.
     *
     * This field is valid when @c status is 0 or 1 (i.e. @c BTYPE is 00).
     */
    uint16_t litlen;
    struct {
      /**
       * When uncompressing a compressed block, log2 of the size of the
       * first-level table in @c litcodes. Always in the range 1 through 9.
       *
       * This field is valid when @c status is in the range 2 through 5 (i.e.
       * @c BTYPE is 01 or 10).
       */
      uint8_t litbits;

      /**
       * When uncompressing a compressed block, log2 of the size of the
       * first-level table in @c distcodes. Always in the range 0 through 6.
       *
       * This field is valid when @c status is in the range 2 through 5 (i.e.
       * @c BTYPE is 01 or 10).
       */
      uint8_t distbits;
    };
  };

  /**
   * The CRC32 (without post-conditioning) of bytes <tt>[0, writepos)</tt> of
   * the uncompressed bytes from the current gzip file.
   */
  uint32_t crc;

  /**
   * Between 0 and 63 bits which have been read from the input, but not yet
   * been used. Starting at the LSB, bits 0 through <tt>nbits - 1</tt> are
   * valid. Bits <tt>nbits</tt> through 63 are zero.
   */
  uint64_t bits;

  /**
   * @brief Circular buffer of uncompressed bytes from the current gzip file.
   *
   * The next uncompressed byte to be consumed by @c pigz_consume is at index
   * <tt>readpos % PIGZ_WINDOW_SIZE</tt>. The next byte to be overwritten by
   * more uncompressed data is at <tt>writepos % PIGZ_WINDOW_SIZE</tt>.
   */
  char window[PIGZ_WINDOW_SIZE];

  /**
   * @brief Lookup tables for interpreting Huffman-encoded distance codes.
   *
   * Each entry is of the form <tt>(VAL << 16) | (NBITS << 8) | (KIND << 6) | NXBITS</tt>,
   * where @c NXBITS is a 6-bit value, @c KIND is a 2-bit value, @c NBITS is
   * an 8-bit value, and @c VAL is a 16-bit value. @c NBITS gives the number
   * of bits which should be dropped from the input bitstream. @c NXBITS gives
   * the number of bits which should be consumed from the input bitstream,
   * treated as an <tt>NXBIT</tt>-integer, and added to @c VAL. The meaning of
   * @c VAL then depends on @c KIND: <ul>
   *  <li>If @c KIND is 0, @c VAL gives a distance value.</li>
   *  <li>If @c KIND is 1 or 2, the bitstream is bad.</li>
   *  <li>If @c KIND is 3, @c VAL gives the index of another entry in @c distcodes.
   *      In this case, the @c NXBITS bits are peeked rather than consumed.</li>
   * </ul>
   *
   * The first <tt>pow(2, distbits)</tt> entries are called the first-level
   * table. This first-level table is followed by zero or more second-level
   * tables. Second-level tables are only used if there distance codes which
   * are encoded using more than six bits; the leading six bits will lead to an
   * entry in the first-level table with a @c KIND of 3 and a @c VAL (before
   * adding in @c NXBITS bits) which gives the offset of the start of a
   * second-level table, the size of which is <tt>pow(2, NXBITS)</tt> (hence
   * once the @c NXBITS bits are added to @c VAL, the result is the offset
   * of a particular element in the referenced second-level table).
   */
  uint32_t distcodes[592];

  /**
   * @brief Lookup tables for interpreting Huffman-encoded literal/length codes.
   *
   * Like @c distcodes, but with different meanings for @c KIND: <ul>
   *  <li>If @c KIND is 0, @c VAL gives a length value (which is then followed
   *      by a Huffman-encoded distance code).</li>
   *  <li>If @c KIND is 1, @c VAL gives a literal value.</li>
   *  <li>If @c KIND is 2 and @c VAL is zero, end-of-block has been reached.</li>
   *  <li>If @c KIND is 2 and @c VAL is non-zero, the bitstream is bad.</li>
   *  <li>If @c KIND is 3, @c VAL gives the index of another entry in @c litcodes.
   *      In this case, the @c NXBITS bits are peeked rather than consumed.</li>
   * </ul>
   *
   * If @c KIND is 1 or 2, @c NXBITS is always zero. The first <tt>pow(2, litbits)</tt>
   * entries are called the first-level table. As for @c distcodes, second-level
   * tables might follow, albeit with the cutoff at nine bits rather than six bits.
   */
  uint32_t litcodes[852];
} pigz_state;

/**
 * @brief Initialise (or re-initialise) a pigz_state structure.
 *
 * Following initialisation, the @c status field of @p state will be
 * non-negative. Initialisation does not call the @p reader function - 
 * that will happen on the next call to @c pigz_available. Initialisation
 * does not allocate any resources, and cannot fail. There is no corresponding
 * de-initialisation function, as such a function is not required.
 *
 * @param state The state to initialise.
 * @param opaque An opaque value which will later be passed to @p reader.
 * @param reader The function which will be called by @c pigz_available to get
 *               chunks of input (i.e. chunks of gzip file).
 */
void pigz_init(pigz_state* state, void* opaque, pigz_reader reader);

/**
 * @brief Determine how many uncompressed bytes are available to be consumed.
 *
 * Following a call to @c pigz_available which returns @c N, it is permitted to
 * call @c pigz_consume passing @c N, or call @c pigz_consume multiple times
 * with values which sum to @c N. If bytes are available, @c pigz_available
 * will return a non-negative value, and the @c status field of @p state will
 * be set to some (unrelated) non-negative value. Otherwise, if the end of
 * input is reached, @c pigz_available will return zero, and the @c status
 * field of @p state will be set to @c PIGZ_STATUS_EOF (a negative value).
 * Otherwise, if an error is detected in the input stream, @c pigz_available
 * will return zero, and the @c status field of @p state will be set to some
 * other negative value describing the error (@c PIGZ_STATUS_UNEXPECTED_EOF,
 * @c PIGZ_STATUS_BAD_HEADER, @c PIGZ_STATUS_BAD_CRC, or
 * @c PIGZ_STATUS_BAD_BITS).
 *
 * If @c pigz_available returns zero for a given state, then subsequent calls
 * for that state will also return zero (i.e. once EOF is reached, more data
 * cannot be provided, and once an error occurs, it cannot be cleared).
 *
 * @c pigz_available will only return zero in case of EOF or error. It may
 * call the @c reader function previously provided to @c pigz_init in order
 * to fetch more compressed data, and then decompress that data in order to
 * make available some more uncompressed bytes.
 *
 * @param state A state previously initialised by @c pigz_init.
 * @return The maximum value which can be passed to @c pigz_consume for
 *         @p state. Zero in case of EOF or error.
 */
uint64_t pigz_available(pigz_state* state);

/**
 * @brief Read some number of uncompressed bytes.
 *
 * @param len The number of bytes to read. Must be less than or equal to the
 *            result of @c pigz_available for @p state.
 * @return A pointer to the start of the uncompressed bytes (if say @c ptr is
 *         returned, then the uncompressed bytes are the half-open range
 *         <tt>[ptr, ptr + len)</tt>). This pointer remains valid until the
 *         next call to @c pigz_available for @p state (after such a call, the
 *         contents of the range might be replaced with other data).
 */
static const char* pigz_consume(pigz_state* state, uint64_t len) {
  char* result = state->window + (state->readpos & (PIGZ_WINDOW_SIZE - 1));
  state->readpos += len;
  return result;
}

#ifdef __cplusplus
}
#endif
