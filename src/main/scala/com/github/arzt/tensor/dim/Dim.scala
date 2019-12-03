package com.github.arzt.tensor
package dim

import collection.immutable.IndexedSeq

sealed trait Dim extends IndexedSeq[Int] {

  def subIndices: Seq[Int]

  def shape: List[Int]

  def n: Int

  def originalLength: Int

  override def iterator: Iterator[Int] =
    (0 until length).iterator.map(apply)

}

object Dim {

  def fromDimensions(ns: List[Int]): Dim =
    ns match {
      case List(n) => Dim(n)
      case n :: tail => Dim(n, fromDimensions(tail))
    }

  def apply(ns: List[(Int, Seq[Int])]): Dim =
    ns match {
      case List((n, seq)) => Dim(n, seq)
      case (n, seq) :: tail => Dim(n, seq, Dim(tail))
    }

  def apply(n: Int, is: Seq[Int]): UnitDim =
    new UnitDim(n, is)

  def apply(n: Int): UnitDim =
    new UnitDim(n, 0 until n)

  def apply[K <: Dim](n: Int, is: Seq[Int], child: K): RecDim[K] =
    new RecDim[K](n, is, child)

  def apply[K <: Dim](n: Int, child: K): RecDim[K] =
    new RecDim[K](n, 0 until n, child)

}

class UnitDim(val n: Int, val subIndices: Seq[Int]) extends Dim {

  override val shape: List[Int] = List(subIndices.length)

  override def apply(i: Int): Int =
    subIndices(i)

  override val length: Int =
    subIndices.length

  override val originalLength: Int = n

}

class RecDim[K <: Dim](
    val n: Int,
    val subIndices: Seq[Int],
    child: K) extends Dim {

  override val shape: List[Int] = subIndices.length :: child.shape

  override val length: Int = child.length * subIndices.length

  override val originalLength: Int = n * child.originalLength

  override def apply(i: Int): Int = {
    if (i < 0 || i >= length) {
      throw new IndexOutOfBoundsException()
    } else {
      val j = i % child.length
      val m = i / child.length
      child(j) + child.originalLength * subIndices(m)
    }
  }

}
