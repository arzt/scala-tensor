package com.github.arzt.tensor.convert

class LongToIntConverter extends Converter[Long, Int] {

  val n = 2

  val zero = 0L

  val targetSize = 32

  override def read(a: Long, i: Int): Int =
    Converter.readLong(targetSize, a, n - i - 1).toInt

  override def write(a: Long, i: Int, t: Int): Long =
    Converter.writeLong(targetSize, a, n - i - 1, t)
}
