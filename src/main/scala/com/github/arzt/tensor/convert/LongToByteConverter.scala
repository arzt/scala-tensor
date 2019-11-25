package com.github.arzt.tensor.convert

import com.github.arzt.tensor.convert.Converter.readLong
import com.github.arzt.tensor.convert.Converter.writeLong

class LongToByteConverter extends Converter[Long, Byte] {

  val n = 8

  val zero = 0L

  def read(s: Long, i: Int): Byte =
    readLong(8, s, toBigEndian(i)).toByte

  def write(s: Long, i: Int, t: Byte): Long =
    writeLong(8, s, toBigEndian(i), t)

}
