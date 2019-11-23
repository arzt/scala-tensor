package com.github.arzt.tensor.convert

import com.github.arzt.tensor.convert.Converter.readLong
import com.github.arzt.tensor.convert.Converter.writeLong

class ByteToBooleanConverter extends Converter[Byte, Boolean] {

  val zero = 0.toByte

  override val n = 8

  override def read(s: Byte, i: Int): Boolean =
    (readLong(1, s, toBigEndian(i)) & 1) == 1

  override def write(s: Byte, i: Int, t: Boolean): Byte =
    writeLong(1, s, toBigEndian(i), if (t) 1 else 0).toByte
}
