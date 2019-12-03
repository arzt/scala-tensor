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

}
