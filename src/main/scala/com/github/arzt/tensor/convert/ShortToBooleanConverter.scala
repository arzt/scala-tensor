package com.github.arzt.tensor.convert

import com.github.arzt.tensor.convert.Converter.readLong
import com.github.arzt.tensor.convert.Converter.writeLong

class ShortToBooleanConverter extends Converter[Short, Boolean] {

  val n = 16

  val zero: Short = 0

  def read(s: Short, i: Int): Boolean =
    (readLong(1, s, toBigEndian(i)) & 1) == 1

  def write(s: Short, i: Int, t: Boolean): Short =
    writeLong(1, s, toBigEndian(i), if (t) 1 else 0).toShort

}
