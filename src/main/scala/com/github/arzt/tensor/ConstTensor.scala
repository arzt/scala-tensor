package com.github.arzt.tensor

import scala.collection.immutable.Seq

class ConstTensor[T](val shape: Seq[Int], value: T) extends Tensor[T] {
  override def isView: Boolean = true

  override def apply(a: Int): T = value

  override def update(a: Int, v: T): Unit =
    throw new UnsupportedOperationException("update unsupported on const tensor")
}
