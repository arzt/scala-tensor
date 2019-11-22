package com.github.arzt.tensor.convert

import com.github.arzt.tensor.convert.Converter.readLong

class LongToIntConverter extends Converter[Long, Int] {

  val n = 2

  val zero = 0L

  override def read(a: Long, i: Int): Int =
    readLong(32, a, n - i - 1).toInt

  override def write(a: Long, i: Int, t: Int): Long = {
    Converter.writeLong(32, a, n - i - 1, t)
  }
}
