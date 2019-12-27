package com.github.arzt.tensor
import collection.immutable.Seq

class DiagTensor[T](val shape: Seq[Int], tensor: Tensor[T], default: T) extends Tensor[T] {

  override def isView: Boolean = true

  override def apply(a: Int): T = {
    val prod = shape.last * shape(shape.length - 2)
    val rows = shape(shape.length - 2)
    val cols = shape(shape.length - 1)
    val offset = a / prod
    val rem = a % prod
    val tmp = DiagTensor.mapping(rows, cols)(rem)
    if (tmp == -1)
      default
    else
      tensor(offset * tensor.shape.last + tmp)
  }

  override def update(a: Int, v: T): Unit =
    throw new UnsupportedOperationException("DiagTensor is not writable")
}

object DiagTensor {

  def mapping(rows: Int, cols: Int): Int => Int =
    i => {
      val row = i / rows
      val col = i % rows
      if (row == col)
        row
      else
        -1
    }

}