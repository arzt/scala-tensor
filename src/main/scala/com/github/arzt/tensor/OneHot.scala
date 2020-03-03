package com.github.arzt.tensor

import scala.collection.immutable.Seq

class OneHot[T](
    val shape: Seq[Int],
    val one: T,
    val zero: T,
    val hot: Int) extends Tensor[T] {

  override def isView: Boolean = true

  override def apply(a: Int): T = if (a == hot) one else zero

  override def update(a: Int, v: T): Unit = throw new UnsupportedOperationException("update no supported")
}

object OneHot {
  def apply[T](shape: Seq[Int], one: T, zero: T, hot: Int): Tensor[T] =
    new OneHot(shape, one, zero, hot)

  def ofDouble(shape: Seq[Int], hot: Int): Tensor[Double] = apply(shape, 1.0, 0.0, hot)
}