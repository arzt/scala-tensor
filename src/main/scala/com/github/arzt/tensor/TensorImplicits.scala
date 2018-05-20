package com.github.arzt.tensor

import scala.reflect.ClassTag

import scala.languageFeature.implicitConversions

import scala.language.implicitConversions

object TensorImplicits {

  case class WithOffset[T: ClassTag] private (data: Array[T], offset: Int) {
    def asTensor(dim: Int*): Tensor[T] =
      Tensor[T](dim.toVector, data, offset)
  }

  implicit class ArrayOps[T: ClassTag](data: Array[T]) {

    def asTensor(dim: Int*): Tensor[T] = Tensor(dim.toVector, data)

    def asRows(rows: Int): Tensor[T] = asTensor(rows, data.length / rows)

    def asCols(cols: Int): Tensor[T] = asTensor(data.length / cols, cols)

    def asRow: Tensor[T] = asTensor(data.length)

    def asCol: Tensor[T] = asCols(1)

    def withOffset(offset: Int): WithOffset[T] = WithOffset(data, offset)
  }

  implicit class BooleanTensorOps(tensor: Tensor[Boolean]) {
    def unary_! : Tensor[Boolean] = tensor.map(!_)

    def &&(b: Boolean): Tensor[Boolean] = tensor.map(_ && b)

    def ||(b: Boolean): Tensor[Boolean] = tensor.map(_ || b)

    def ^(b: Boolean): Tensor[Boolean] = tensor.map(_ ^ b)

    def &&(that: Tensor[Boolean]): Tensor[Boolean] = tensor.combine[Boolean, Boolean](that, _ && _)

    def ||(that: Tensor[Boolean]): Tensor[Boolean] = tensor.combine[Boolean, Boolean](that, _ || _)

    def ^(that: Tensor[Boolean]): Tensor[Boolean] = tensor.combine[Boolean, Boolean](that, _ ^ _)
  }

  implicit class NumericTensorOps[T](tensor: Tensor[T])(implicit num: Numeric[T]) {

    def +(a: T): Tensor[T] = tensor.map(num.plus(_, a))

    def +(a: Tensor[T]): Tensor[T] = tensor.combine[T, T](a, (x, y) => num.plus(x, y))

    def -(a: T)(implicit num: Numeric[T]): Tensor[T] = tensor.map(num.minus(_, a))

    def -(a: Tensor[T])(implicit num: Numeric[T]): Tensor[T] = tensor.combine[T, T](a, num.minus)

    def *(a: T): Tensor[T] = tensor.map(x => num.times(x, a))

    def *(a: Tensor[T]): Tensor[T] = tensor.combine[T, T](a, num.times)

  }

  implicit def int2Index(i: Int): Index = dim => Seq(((i % dim) + dim) % dim)

  implicit def seq2Index(seq: Seq[Int]): Index = _ => seq

  implicit def bool2index(seq: Seq[Boolean]): Index = dimSize => {
    Iterator
      .continually(seq)
    assert(dimSize == seq.size)
    (0 until dimSize).view.filter(seq)
  }

  implicit def tripleToIndex(tripple: (Int, Int, Int)): Index =
    dim => {
      val (from, to, by) = tripple
      val fromNorm = ((dim + from) % dim + dim) % dim
      val toNorm = ((dim + to) % dim + dim) % dim
      Range.inclusive(fromNorm, toNorm, by)
    }

  implicit def intTensorToIndex(t: Tensor[Int]): Index = t.toSeq

  implicit def boolTensorToIndex(t: Tensor[Boolean]): Index = t.toSeq

  //implicit def to[T](in: Tensor[T]): T = in.apply(0)

  val $colon$colon: Index = dimSize => 0 until dimSize

  val $minus$colon$colon: Index = dimSize => (dimSize - 1) to 0 by -1

  implicit class IntOps(b: Int) {
    def ::(a: Int): (Int, Int, Int) = (a, b, 1)
  }

  implicit class TrippleOps(t: (Int, Int, Int)) {
    def ::(a: Int): (Int, Int, Int) = (a, t._1, t._2)
  }
}
