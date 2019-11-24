package com.github.arzt.tensor.dim

import com.github.arzt.math.generalizedMod

sealed trait Dim extends Seq[Int] {

  def dimIndices: Seq[Int]

  def shape: List[Int]

  def n: Int

  val dim: Int = dimIndices.length

  override def iterator: Iterator[Int] =
    (0 until length).iterator.map(apply)

}

object Dim {

  def apply(ns: List[Int]): Dim =
    ns match {
      case List(n) => Dim(n)
      case n :: tail => Dim(n, Dim(tail))
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

class UnitDim(val n: Int, val dimIndices: Seq[Int]) extends Dim {

  override val shape: List[Int] = List(dim)

  override def apply(i: Int): Int =
    generalizedMod(dimIndices(i), n)

  override def length: Int =
    dimIndices.length

  def apply(seq: Seq[Int]): UnitDim =
    new UnitDim(n, seq)

}

class RecDim[K <: Dim](
    val n: Int,
    val dimIndices: Seq[Int],
    child: K) extends Dim {

  override val shape: List[Int] = dim :: child.shape

  override def length: Int = child.length * dim

  override def apply(i: Int): Int = {
    if (i < 0 || i >= length) {
      throw new IndexOutOfBoundsException()
    } else {
      val j = i % child.length
      val m = i / child.length
      child(j) + child.length * generalizedMod(dimIndices(m), n)
    }
  }

  def sub(x: Seq[Int]): RecDim[K] = Dim(n, x, child)

  def apply(x: Seq[Int], f: K => K = identity): RecDim[K] = Dim[K](n, x, f(child))

}
