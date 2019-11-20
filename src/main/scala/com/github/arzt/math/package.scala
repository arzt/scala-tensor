package com.github.arzt

package object math {

  def generalizedMod(j: Int, n: Int): Int =
    if (j > -1)
      j % n
    else
      (j % n + n) % n

}
