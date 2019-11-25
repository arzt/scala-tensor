package com.github.arzt.tensor.convert

import com.github.arzt.tensor.convert.Converter.readLong
import com.github.arzt.tensor.convert.Converter.writeLong

class ShortToByteConverter extends Converter[Short, Byte] {

  val n = 2

  val zero: Short = 0

  def read(s: Short, i: Int): Byte =
    readLong(8, s, toBigEndian(i)).toByte

  def write(s: Short, i: Int, t: Byte): Short =
    writeLong(8, s, toBigEndian(i), t).toShort

}
