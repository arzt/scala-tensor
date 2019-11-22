package com.github.arzt.tensor.convert

trait Converter[S, T] {

  def zero: S

  def n: Int

  def read(s: S, i: Int): T

  def write(s: S, i: Int, t: T): S
}

object Converter {
  def readLong(size: Int, s: Long, i: Int): Long = {
    s >> (i * size)
  }

  def readInt(size: Int, s: Int, i: Int): Int = {
    s >> (i * size)
  }

  def readBooleanInt(s: Int, i: Int): Boolean = {
    ((s >> i) & 1) == 1
  }

  def writeLong(size: Int, s: Long, i: Int, t: Long): Long = {
    val ones = ((1L << size) - 1) << (i * size)
    val zeros = ~ones
    val tNew = t << (i * size)
    s & zeros | tNew & ones
  }

  def writeInt(size: Int, s: Int, i: Int, t: Int): Int = {
    val ones = ((1 << size) - 1) << (i * size)
    val zeros = ~ones
    val tNew = t << (i * size)
    s & zeros | tNew & ones
  }
}