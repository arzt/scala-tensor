package com.github.arzt.tensor.convert

import com.github.arzt.tensor.convert.Converter.readLong
import com.github.arzt.tensor.convert.Converter.writeLong

class LongToByteConverter extends Converter[Long, Byte] {

  val n = 8

  val zero = 0

  def read(s: Long, i: Int): Byte =
    readLong(8, s, n - i - 1).toByte

  def write(s: Long, i: Int, t: Byte): Long =
    writeLong(8, s, n - i - 1, t)
}
