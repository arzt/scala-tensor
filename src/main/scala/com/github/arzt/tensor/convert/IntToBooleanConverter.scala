package com.github.arzt.tensor.convert

import com.github.arzt.tensor.convert.Converter.readLong
import com.github.arzt.tensor.convert.Converter.writeLong

class IntToBooleanConverter extends Converter[Int, Boolean] {

  val n = 32

  val zero = 0

  override def read(s: Int, i: Int): Boolean =
    (readLong(1, s, toBigEndian(i)) & 1) == 1

  override def write(s: Int, i: Int, t: Boolean): Int =
    writeLong(1, s, toBigEndian(i), if (t) 1 else 0).toInt
}
