package com.github.arzt.tensor

import scala.collection.immutable
import scala.reflect.ClassTag

private class MergeTensor[T](
    val shape: immutable.Seq[Int],
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

  override def toSeq: Seq[T] = (0 until length).map(apply)
}

object MergeTensor {

  private[tensor] def getShape[T](t: Tensor[Tensor[T]]): immutable.Seq[Int] = {
    val shapes = t.map(_.shape)
    val n = t.shape.length
    (0 until n)
      .foldLeft(shapes) {
        case (tmp, i) => tmp
          .dissect(i).map(_.toSeq)
          .map(_.reduce((a, b) => a.updated(i, a(i) + b(i))))
      }
      .apply(0)
  }

  private[tensor] def getParentAndChildMap[T](t: Tensor[Tensor[T]], shape: immutable.Seq[Int]): (Array[Int], Array[Int]) = {
    val width = shape.last
    val n = shape.product
    val pos = new Array[Int](t.length)
    val widths = t.map(_.shape.last).toSeq.toArray
    val lengths = t.map(_.length).toSeq.toArray
    val parentMap = new Array[Int](n)
    val childMap = new Array[Int](n)
    var child = 0
    var i = 0
    val last = (0 until width).toArray
    var lastChild = 0
    while (i < n) {
      parentMap(i) = child
      childMap(i) = pos(child)
      pos(child) += 1

      if (pos(lastChild) == lengths(lastChild)) {
        lastChild += 1
      }

      i += 1

      if (pos(child) % widths(child) == 0) {
        child += 1
      }
      if (i % width == 0) {
        child = lastChild
      }
    }
    (parentMap, childMap)
  }

}