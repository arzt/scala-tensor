package com.github.arzt.tensor

import com.github.arzt.tensor.convert.Converter

import scala.reflect.ClassTag

class ConvertTensor[T, U](val shape: Seq[Int], val parent: Tensor[U], convert: Converter[U, T])(implicit val tag: ClassTag[T]) extends Tensor[T] {
  //override val shape: Seq[Int] = parent.shape :+ convert.n

  override def isView: Boolean = parent.isView

  override def apply(a: Int): T = {
    val j = a / convert.n
    val i = a % convert.n
    val u = parent(j)
    convert.read(u, i)
  }

  override def update(a: Int, v: T): Unit = {
    val j = a / convert.n
    val i = a % convert.n
    parent.update(j, convert.write(parent(j), i, v))
  }

  override def toSeq: Seq[T] = (0 until length).view.map(apply).toSeq
}
