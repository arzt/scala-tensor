package com.github.arzt

package object math {

  /**
   * Computes positive integer number y (remainder) congruent to x modulo n
   * @param x divident
   * @param n divisor
   * @return y remainder
   */
  def positiveMod(n: Int)(x: Int): Int = {
    val m = x % n
    if (m >= 0) m else m + n
  }

  /**
   * Computes unsigned integer representation
   * @param bitWidth Bit width of input integer
   * @param x Input integer
   * @return Unsigned representation
   */
  def asUnsignedInteger(bitWidth: Int)(x: Long): Long =
    ((1L << bitWidth) - 1) & x

  /**
   * Computes signed integer representation
   * @param bitWidth Bit width of input integer
   * @param x Input integer
   * @return Signed representation
   */
  def asSignedInteger(bitWidth: Int)(x: Long): Long =
    (x << (64 - bitWidth)) >> (64 - bitWidth)

}
