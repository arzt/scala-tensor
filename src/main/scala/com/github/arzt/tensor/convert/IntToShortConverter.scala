package com.github.arzt.tensor.convert

import com.github.arzt.tensor.convert.Converter.readLong
import com.github.arzt.tensor.convert.Converter.writeLong

class IntToShortConverter extends Converter[Int, Short] {

  val zero = 0

  val n = 2

  def read(s: Int, i: Int): Short =
    readLong(16, s, toBigEndian(i)).toShort

  def write(s: Int, i: Int, t: Short): Int =
    writeLong(16, s, toBigEndian(i), t).toInt

}
