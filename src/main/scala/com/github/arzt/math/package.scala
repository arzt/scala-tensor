package com.github.arzt

package object math {

  /**
   * Computes positive integer number y (remainder) congruent to x modulo n
   * @param x divident
   * @param n divisor
   * @return y remainder
   */
  def generalizedMod(x: Int, n: Int): Int = {
    val m = x % n
    if (m > -1) m else m + n
  }

}
