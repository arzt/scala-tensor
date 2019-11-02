package com.github.arzt.tensor.dim

trait Dim[+T] extends Seq[Int] {

  def dimIndices: Seq[Int]

  def dims: List[Int]

  def dim: Int

  def n: Int

}

trait RDim[+T] extends Dim[T] {
    def sub(x: Seq[Int]): Dim[T]
}

class UnitDim(val n: Int, val dimIndices: Seq[Int]) extends Dim[Nothing] {

  override def dims: List[Int] = List(length)

  override def apply(i: Int): Int = dimIndices(i)

  override def length: Int = dimIndices.length

  override def iterator: Iterator[Int] = dimIndices.iterator

  override def dim: Int = length

}

class RecDim[K <: Dim[_]](
  val n: Int,
  val dimIndices: Seq[Int],
  child: K
) extends RDim[K] {

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

  override def sub(x: Seq[Int]): Dim[K] = this
}

object Dim {

  def apply(is: Seq[Int]): UnitDim = new UnitDim(is.max, is)

  def apply(n: Int): UnitDim = new UnitDim(n, 0 until n)

  def apply[K <: Dim[_]](n: Int, is: Seq[Int], parent: K): RDim[K] = new RecDim[K](n, is, parent)

  def apply[K<: Dim[_]](n: Int, parent: K): RDim[K] = new RecDim[K](n, 0 until n, parent)
}