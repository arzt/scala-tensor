package com.github.arzt.tensor

import scala.reflect.ClassTag

object TensorImplicits {

  case class WithOffset[T: ClassTag] private(data: Array[T], offset: Int) {
    def asTensorOfDim(dim: Int*): Tensor[T] =
      Tensor[T](dim.toVector, data, offset)
  }

  implicit class ArrayOps[T: ClassTag](data: Array[T]) {

    def asVector(): Tensor[T] = Tensor(Vector(data.length), data)

    def asTensorOfDim(dim: Int*): Tensor[T] = Tensor(dim.toVector, data)

    def asRow: Tensor[T] = Tensor(Vector(data.length), data)

    def asCol: Tensor[T] = Tensor(Vector(data.length, 1), data)

    def withOffset(offset: Int): WithOffset[T] = WithOffset(data, offset)
  }

  implicit class BooleanTensorOps(tensor: Tensor[Boolean]) {
    def unary_! : Tensor[Boolean] = tensor.map(!_)

    def !=(b: Boolean): Tensor[Boolean] = tensor.map(_ != b)

    def &&(b: Boolean): Tensor[Boolean] = tensor.map(_ && b)

    def ||(b: Boolean): Tensor[Boolean] = tensor.map(_ || b)

    def !=(that: Tensor[Boolean]): Tensor[Boolean] = tensor.combine[Boolean, Boolean](that, _ != _)

    def &&(that: Tensor[Boolean]): Tensor[Boolean] = tensor.combine[Boolean, Boolean](that, _ && _)

    def ||(that: Tensor[Boolean]): Tensor[Boolean] = tensor.combine[Boolean, Boolean](that, _ || _)
  }

  implicit class MathOps[T >: AnyVal : ClassTag](tensor: Tensor[T])(implicit numeric: Numeric[T]) {
    def test = tensor.map(x => numeric.minus(x, x))
  }

  implicit def int2Index(i: Int): Index = dim =>
    if (i < 0) Seq(dim + i) else Seq(i)

  implicit def seq2Indexed(seq: Seq[Int]): Index = _ => seq

  implicit def bool2index(seq: Seq[Boolean]): Index = dimSize => {
    assert(dimSize == seq.size)
    (0 until dimSize).view.filter(seq)
  }

  implicit def tripleToIndex(t: (Int, Int, Int)): Index =
    (dim: Int) => t match {
      case (from, to, by) =>
        if (from > -1) {
          if (to > -1) {
            Range.inclusive(from, to, by)
          } else {
            Range.inclusive(from, dim + to, by)
          }
        } else {
          if (to > -1) {
            Range.inclusive(dim + from, to, by)
          } else {
            Range.inclusive(dim + from, dim + to, by)
          }
        }
    }

  implicit def intTensorToIndex(t: Tensor[Int]): Index = t.toSeq

  implicit def boolTensorToIndex(t: Tensor[Boolean]): Index = t.toSeq

  //implicit def to[T](in: Tensor[T]): T = in.apply(0)

  val $colon$colon: Index = dimSize => 0 until dimSize

  implicit class IntOps(b: Int) {
    def ::(a: Int): (Int, Int, Int) = (a, b, 1)
  }

  implicit class TrippleOps(t: (Int, Int, Int)) {
    def ::(a: Int): (Int, Int, Int) = (a, t._1, t._2)
  }

  val a: Index = 1 :: 2 :: 3

  val b: Index = ::

  val c: Index = Seq[Boolean](true, false)

  val d: Index = Seq[Int](1, 2, 3, 4, 0)
}
