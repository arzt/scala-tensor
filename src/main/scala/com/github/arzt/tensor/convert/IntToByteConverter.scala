package com.github.arzt.tensor.convert

import com.github.arzt.tensor.convert.Converter.readLong
import com.github.arzt.tensor.convert.Converter.writeLong

class IntToByteConverter extends Converter[Int, Byte] {

  val zero = 0

  val n = 4

  def read(s: Int, i: Int): Byte =
    readLong(8, s, n - i - 1).toByte

  def write(s: Int, i: Int, t: Byte): Int =
    writeLong(8, s, n - i - 1, t).toInt
}
