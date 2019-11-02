package com.github.arzt.tensor.dim

sealed trait Dim[+T] extends Seq[Int] {

  def dimIndices: Seq[Int]

  def dims: List[Int]

  def dim: Int

  def n: Int

  def sub(x: Seq[Int]): Dim[T]

}

class UnitDim(val n: Int, val dimIndices: Seq[Int]) extends Dim[Nothing] {

  override def dims: List[Int] = List(length)

  override def apply(i: Int): Int = dimIndices(i)

  override def length: Int = dimIndices.length

  override def iterator: Iterator[Int] = dimIndices.iterator

  override def dim: Int = length

  override def sub(x: Seq[Int]): Dim[Nothing] = {

    this
  }
}

class RecDim[K](
  val n: Int,
  val dimIndices: Seq[Int],
  child: Dim[K]
) extends Dim[Dim[K]] {

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

  override def sub(x: Seq[Int]): Dim[Dim[K]] = this
}

object Dim {

  def apply(is: Seq[Int]): UnitDim = new UnitDim(is.max, is)

  def apply(n: Int): UnitDim = new UnitDim(n, 0 until n)

  def apply[K](n: Int, is: Seq[Int], parent: Dim[K]): Dim[Dim[K]] = new RecDim[K](n, is, parent)

  def apply[K](n: Int, parent: Dim[K]): Dim[Dim[K]] = new RecDim[K](n, 0 until n, parent)
}