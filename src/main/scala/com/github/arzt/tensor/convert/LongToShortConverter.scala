package com.github.arzt.tensor.convert

import com.github.arzt.tensor.convert.Converter.readLong
import com.github.arzt.tensor.convert.Converter.writeLong

class LongToShortConverter extends Converter[Long, Short] {

  val n = 4

  val zero = 0

  override def read(a: Long, i: Int): Short =
    readLong(16, a, n - i - 1).toShort

  override def write(a: Long, i: Int, t: Short): Long =
    writeLong(16, a, n - i - 1, t)
}
