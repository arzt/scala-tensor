package com.github.arzt.tensor.convert

trait Converter[S, T] {

  def zero: S

  def n: Int

  def read(s: S, i: Int): T

  def write(s: S, i: Int, t: T): S

  @inline
  final def toBigEndian(i: Int): Int =
    n - i - 1
}

object Converter {

  def readLong(size: Int, s: Long, i: Int): Long = {
    s >> (i * size)
  }

  def writeLong(size: Int, s: Long, i: Int, t: Long): Long = {
    val ones = ((1L << size) - 1) << (i * size)
    val zeros = ~ones
    val tNew = t << (i * size)
    s & zeros | tNew & ones
  }

}
