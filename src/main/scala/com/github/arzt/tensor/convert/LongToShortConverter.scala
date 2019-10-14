package com.github.arzt.tensor.convert

import com.github.arzt.tensor.convert.Converter.readLong
import com.github.arzt.tensor.convert.Converter.writeLong

class LongToShortConverter extends Converter[Long, Short] {

  val n = 4

  val zero = 0

  val targetSize = 16

  override def read(a: Long, i: Int): Short =
    readLong(targetSize, a, n - i - 1).toShort

  override def write(a: Long, i: Int, t: Short): Long =
    writeLong(targetSize, a, n - i - 1, t)
}
