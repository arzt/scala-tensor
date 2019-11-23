package com.github.arzt.tensor.convert

import com.github.arzt.tensor.convert.Converter.readLong
import com.github.arzt.tensor.convert.Converter.writeLong

class LongToIntConverter extends Converter[Long, Int] {

  val n = 2

  val zero = 0L

  override def read(a: Long, i: Int): Int =
    readLong(32, a, toBigEndian(i)).toInt

  override def write(a: Long, i: Int, t: Int): Long =
    writeLong(32, a, toBigEndian(i), t)

}
