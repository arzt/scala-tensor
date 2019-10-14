package com.github.arzt.tensor.convert

import com.github.arzt.tensor.convert.Converter.readInt
import com.github.arzt.tensor.convert.Converter.writeInt

class IntToBooleanConverter extends Converter[Int, Boolean] {

  val n = 32

  val zero = 0

  val targetSize = 1

  override def read(s: Int, i: Int): Boolean =
    (readInt(1, s, n - i - 1) & 1) == 1

  override def write(s: Int, i: Int, t: Boolean): Int =
    writeInt(1, s, n - i - 1, if (t) 1 else 0)
}
