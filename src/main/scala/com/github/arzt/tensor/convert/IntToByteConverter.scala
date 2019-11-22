package com.github.arzt.tensor.convert

import com.github.arzt.tensor.convert.Converter.readInt

class IntToByteConverter extends Converter[Int, Byte] {

  val zero = 0

  val n = 4

  def read(s: Int, i: Int): Byte =
    readInt(8, s, n - i - 1).toByte

  def write(s: Int, i: Int, t: Byte): Int =
    Converter.writeInt(8, s, n - i - 1, t)
}