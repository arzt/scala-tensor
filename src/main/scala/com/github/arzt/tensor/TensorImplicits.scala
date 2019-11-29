package com.github.arzt.tensor

import java.awt.image.BufferedImage

import com.github.arzt.math.generalizedMod
import com.github.arzt.tensor.image.ImageTool
import com.github.arzt.tensor.image.ImageTool.fromImage
import com.github.arzt.tensor.op.DoubleTensorMultiplication
import com.github.arzt.tensor.op.FloatTensorMultiplication

import scala.collection.compat.immutable.ArraySeq.unsafeWrapArray
import scala.language.implicitConversions
import scala.languageFeature.implicitConversions
import scala.reflect.ClassTag

object TensorImplicits {

  case class WithOffset[T: ClassTag] private (data: Array[T], offset: Int) {
    def asTensor(dim: Int*): Tensor[T] =
      Tensor[T](offset, data, dim: _*)
  }

  implicit class ArrayOps[T: ClassTag](data: Array[T]) {

    def asTensor(dim: Int*): Tensor[T] = Tensor(data, dim: _*)

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

  implicit class ByteTensorOps(tensor: Tensor[Byte]) {
    def asImage(imageType: Int): BufferedImage = {
      ImageTool.toImage(tensor, imageType)
    }
  }

  def getOp[T](t: Tensor[T]): Char =
    if (t.isInstanceOf[TransposeTensor[T]]) 't' else 'n'

  def getOffset[T](t: Tensor[T]): Int =
    t match {
      case array: ArrayTensor[T] =>
        array.offset
      case transpose: TransposeTensor[T] =>
        transpose.tensor match {
          case array2: ArrayTensor[T] =>
            array2.offset
          case _ =>
            0
        }
      case _ =>
        0
    }

  implicit class NumericTensorOps[T: ClassTag](tensor: Tensor[T])(implicit num: Numeric[T]) {

    def +(a: T): Tensor[T] = tensor.map(num.plus(_, a))

    def +(a: Tensor[T]): Tensor[T] = tensor.combine[T, T](a, (x, y) => num.plus(x, y))

    def -(a: T)(implicit num: Numeric[T]): Tensor[T] = tensor.map(num.minus(_, a))

    def -(a: Tensor[T])(implicit num: Numeric[T]): Tensor[T] = tensor.combine[T, T](a, num.minus)

    def *(a: T): Tensor[T] = tensor.map(x => num.times(x, a))

    def *(a: Tensor[T]): Tensor[T] = tensor.combine[T, T](a, num.times)

  }

  implicit class ConcatOps[T: ClassTag](tensor: Tensor[Tensor[T]]) {
    def concatenate(): Tensor[T] = {
      val shape = MergeTensor.getShape[T](tensor)
      val (parentMap, childMap) = MergeTensor.getParentAndChildMap[T](tensor, shape)
      new MergeTensor[T](shape, tensor, parentMap, childMap)
    }
  }

  implicit def int2Index(i: Int): Index =
    n => Seq(generalizedMod(i, n))

  implicit def iterable2Index(seq: Iterable[Int]): Index =
    _ => seq

  implicit def bool2index(seq: Int => Boolean): Index =
    n => (0 until n).view.filter(seq)

  implicit def tripleToIndex(triple: (Int, Int, Int)): Index =
    n => {
      val (from, to, by) = triple
      val fromNorm = generalizedMod(from, n)
      val toNorm = generalizedMod(to, n)
      Range.inclusive(fromNorm, toNorm, by)
    }

  implicit def intTensorToIndex(t: Tensor[Int]): Index =
    _ => unsafeWrapArray(t.getData)

  implicit def boolTensorToIndex(t: Tensor[Boolean]): Index =
    _ => unsafeWrapArray((0 until t.length).filter(t.apply).toArray)

  val $colon$colon: Index = dimSize => 0 until dimSize

  val $minus$colon$colon: Index =
    dimSize => (dimSize - 1) to 0 by -1

  implicit class IntOps(b: Int) {
    def ::(a: Int): (Int, Int, Int) = (a, b, 1)
  }

  implicit class TrippleOps(t: (Int, Int, Int)) {
    def ::(a: Int): (Int, Int, Int) = (a, t._1, t._2)
  }

  implicit class BufferedImageOps(image: BufferedImage) {
    def asTensor: Tensor[Byte] = fromImage(image)
  }

  implicit val dtM = DoubleTensorMultiplication

  implicit val ftM = FloatTensorMultiplication

}
