package com.github.arzt.tensor

import scala.collection.immutable

class NewIndexTensor(indexShape: immutable.Seq[Int]) extends Tensor[Int] {
  override def shape: Seq[Int] = indexShape.appended(indexShape.length)

  override def isView: Boolean = true

  override def apply(a: Int): Int = {
    a
  }
}

object NewIndexTensor {

  def unindex(shape: List[Int], i: Int, j: Int): Int = {
    shape match {
      case Nil =>
        j
      case head :: tail =>
        unindex(tail, i / head, j - 1)
    }
  }

  def apply(dims: Int*): NewIndexTensor = {
    new NewIndexTensor(dims)
  }
}