package com.github.arzt.tensor.convert

import com.github.arzt.tensor.convert.Converter.readInt
import com.github.arzt.tensor.convert.Converter.writeInt

class IntToShortConverter extends Converter[Int, Short] {

  val zero = 0

  val n = 2

  def read(s: Int, i: Int): Short =
    readInt(16, s, n - i - 1).toShort

  def write(s: Int, i: Int, t: Short): Int =
    writeInt(16, s, n - i - 1, t)
}
