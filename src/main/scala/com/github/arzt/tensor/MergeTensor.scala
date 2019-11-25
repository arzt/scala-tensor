package com.github.arzt.tensor

import scala.collection.immutable.Seq
import scala.reflect.ClassTag

private class MergeTensor[T](
    val shape: Seq[Int],
    parent: Tensor[Tensor[T]],
    parentMap: Int => Int,
    childMap: Int => Int)(implicit val tag: ClassTag[T]) extends Tensor[T] {

  override def isView: Boolean = true

  override def apply(i: Int): T = {
    val pi = parentMap(i)
    val ci = childMap(i)
    val child = parent(pi)
    val item = child(ci)
    item
  }

  override def update(i: Int, v: T): Unit =
    parent(parentMap(i))(childMap(i)) = v

}

object MergeTensor {

  private[tensor] def getShape[T](t: Tensor[Tensor[T]]): Seq[Int] = {
    val shapes = t.map(_.shape)
    val n = t.shape.length
    (0 until n)
      .foldLeft(shapes) {
        case (tmp, i) => tmp
          .dissect(i)
          .map(_.toIterable)
          .map(_.reduce((a, b) => a.updated(i, a(i) + b(i))))
      }
      .apply(0)
  }

  private[tensor] def getParentAndChildMap[T](t: Tensor[Tensor[T]], shape: Seq[Int]): (Array[Int], Array[Int]) = {
    val width = shape.last
    val tWidth = t.shape.last
    val n = shape.product
    val pos = new Array[Int](t.length)
    val widths = t.map(_.shape.last).toArray
    val lengths = t.map(_.length).toArray
    val parentMap = new Array[Int](n)
    val childMap = new Array[Int](n)
    val link = (0 until tWidth).toArray
    var i = 0
    var c = 0
    while (i < n) {
      val child = link(c)
      parentMap(i) = child
      childMap(i) = pos(child)
      pos(child) += 1

      if (pos(child) == lengths(child)) {
        link(c) += tWidth
      }
      if (pos(child) % widths(child) == 0) {
        c += 1
        c %= tWidth
      }
      i += 1
    }
    (parentMap, childMap)
  }

}
