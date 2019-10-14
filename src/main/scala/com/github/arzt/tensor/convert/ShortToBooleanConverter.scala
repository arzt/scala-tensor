package com.github.arzt.tensor.convert

import com.github.arzt.tensor.convert.Converter.readInt
import com.github.arzt.tensor.convert.Converter.writeInt

class ShortToBooleanConverter extends Converter[Short, Boolean] {

  val n = 16

  val zero = 0

  val targetSize = 1

  def read(s: Short, i: Int): Boolean =
    (readInt(targetSize, s, n - i + 1) & 1) == 1

  def write(s: Short, i: Int, t: Boolean): Short =
    writeInt(targetSize, s, n - i + 1, if (t) 1 else 0).toShort
}
