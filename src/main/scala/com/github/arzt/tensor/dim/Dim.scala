package com.github.arzt.tensor.dim

sealed trait Dim extends Seq[Int] {

  def dimIndices: Seq[Int]

  def dims: List[Int]

  def dim: Int

  def n: Int

}

class UnitDim(val n: Int, val dimIndices: Seq[Int]) extends Dim {

  override def dims: List[Int] = List(length)

  override def apply(i: Int): Int = dimIndices(i)

  override def length: Int = dimIndices.length

  override def iterator: Iterator[Int] = dimIndices.iterator

  override def dim: Int = length

}

class RecDim[K <: Dim](
  val n: Int,
  val dimIndices: Seq[Int],
  child: K
) extends Dim {

  override def dims: List[Int] = dim :: child.dims

  override def dim: Int = dimIndices.length

  override def length: Int = child.length * dimIndices.length

  override def iterator: Iterator[Int] = (0 to length).iterator.map(apply)

  override def apply(i: Int): Int = {
    val j = i % dim
    val m = i / dim
    val y = child(j) + m * dim
    y
  }

  def sub(x: Seq[Int]): RecDim[K] = this
}

object Dim {

  def apply(is: Seq[Int]): UnitDim = new UnitDim(is.max, is)

  def apply(n: Int): UnitDim = new UnitDim(n, 0 until n)

  def apply[K <: Dim](n: Int, is: Seq[Int], parent: K): RecDim[K] = new RecDim[K](n, is, parent)

  def apply[K<: Dim](n: Int, parent: K): RecDim[K] = new RecDim[K](n, 0 until n, parent)
}