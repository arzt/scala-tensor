package com.github.arzt.tensor

import collection.immutable.Seq

class DiagTensor[T](val shape: Seq[Int], tensor: Tensor[T], default: T) extends Tensor[T] {

  override def isView: Boolean = true

  private val prod = shape.last * shape.last

  override def apply(a: Int): T = {
    val offset = a / prod
    val rem = a % prod
    val i = DiagTensor.mapping(shape.last)(rem)
    if (i == -1)
      default
    else
      tensor(offset * shape.last + i)
  }

}

object DiagTensor {

  def mapping(rows: Int): Int => Int =
    i => {
      val row = i / rows
      val col = i % rows
      if (row == col)
        row
      else
        -1
    }

}
