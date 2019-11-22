package com.github.arzt.tensor.convert

import com.github.arzt.tensor.convert.Converter.readInt
import com.github.arzt.tensor.convert.Converter.writeInt

class ByteToBooleanConverter extends Converter[Byte, Boolean] {

  val zero = 0.toByte

  override val n = 8

  override def read(s: Byte, i: Int): Boolean =
    (readInt(1, s, n - i - 1) & 1) == 1

  override def write(s: Byte, i: Int, t: Boolean): Byte =
    writeInt(1, s, n - i - 1, if (t) 1 else 0).toByte
}
