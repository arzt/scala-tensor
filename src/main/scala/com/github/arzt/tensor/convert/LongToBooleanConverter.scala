package com.github.arzt.tensor.convert

import com.github.arzt.tensor.convert.Converter.readLong
import com.github.arzt.tensor.convert.Converter.writeLong

class LongToBooleanConverter extends Converter[Long, Boolean] {

  val n = 64

  val zero = 0

  def read(s: Long, i: Int): Boolean =
    (readLong(1, s, toBigEndian(i)) & 1) == 1

  def write(s: Long, i: Int, t: Boolean): Long =
    writeLong(1, s, toBigEndian(i), if (t) 1 else 0)
}
