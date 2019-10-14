package com.github.arzt.tensor.convert

class LongToByteConverter extends Converter[Long, Byte] {

  val n = 8

  val zero = 0

  val targetSize = 8

  def read(s: Long, i: Int): Byte =
    Converter.readLong(targetSize, s, n - i - 1).toByte

  def write(s: Long, i: Int, t: Byte): Long =
    Converter.writeLong(targetSize, s, n - i - 1, t)
}
