package com.github.arzt.tensor.convert

import com.github.arzt.tensor.convert.Converter.readInt
import com.github.arzt.tensor.convert.Converter.writeInt

class ShortToByteConverter extends Converter[Short, Byte] {

  val n = 2

  val zero = 0

  val targetSize = 8

  def read(s: Short, i: Int): Byte =
    readInt(targetSize, s, n - i + 1).toByte

  def write(s: Short, i: Int, t: Byte): Short =
    writeInt(targetSize, s, n - i + 1, t).toShort
}
