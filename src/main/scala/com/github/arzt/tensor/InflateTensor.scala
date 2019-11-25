package com.github.arzt.tensor

import com.github.arzt.tensor.convert.Converter

import scala.reflect.ClassTag
import collection.immutable.Seq

class InflateTensor[T, U](val shape: Seq[Int], val parent: Tensor[U])(implicit val tag: ClassTag[T], val convert: Converter[U, T]) extends Tensor[T] {

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

}
