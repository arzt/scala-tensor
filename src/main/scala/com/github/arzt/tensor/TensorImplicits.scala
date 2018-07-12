package com.github.arzt.tensor

import org.jblas.NativeBlas.dgemm
import org.jblas.NativeBlas.sgemm

import scala.language.implicitConversions
import scala.languageFeature.implicitConversions
import scala.reflect.ClassTag

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

  private def getData[T: ClassTag](t: Tensor[T]): Array[T] =
    t match {
      case array: ArrayTensor[T] =>
        array.data
      case transpose: TransposeTensor[T] =>
        transpose.tensor match {
          case array2: ArrayTensor[T] =>
            array2.data
          case _ =>
            transpose
              .tensor()
              .asInstanceOf[ArrayTensor[T]]
              .data
        }
      case _ =>
        t()
          .asInstanceOf[ArrayTensor[T]]
          .data
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

  def dgemmT(a: Tensor[Double], b: Tensor[Double], c: ArrayTensor[Double]): Unit = {
    val n = a.shape.length
    require(n == b.shape.length)
    require(a.shape(n - 1) == b.shape(n - 2))
    val AA = getData(a)
    val BB = getData(b)
    val opA = getOp(a)
    val opB = getOp(b)
    val num = a.shape.take(n - 2).product
    val C = getData(c)
    val M = a.shape(n - 2)
    val N = b.shape.last
    val K = a.shape.last
    val ALPHA = 1
    val BETA = 0
    var AO = getOffset(a)
    var BO = getOffset(b)
    var CO = c.offset
    var i = 0
    while (i < num) {
      dgemm(opB, opA, M, N, K, ALPHA, BB, BO, N, AA, AO, K, BETA, C, CO, N)
      AO += M * K
      BO += K * N
      CO += M * N
      i += 1
    }
  }

  def sgemmT(a: Tensor[Float], b: Tensor[Float], c: ArrayTensor[Float]): Unit = {
    val n = a.shape.length
    require(n == b.shape.length)
    require(a.shape(n - 1) == b.shape(n - 2))
    val AA = getData(a)
    val BB = getData(b)
    val opA = getOp(a)
    val opB = getOp(b)
    val num = a.shape.take(n - 2).product
    val C = getData(c)
    val M = a.shape(n - 2)
    val N = b.shape.last
    val K = a.shape.last
    val ALPHA = 1
    val BETA = 0
    var AO = getOffset(a)
    var BO = getOffset(b)
    var CO = c.offset
    var i = 0
    while (i < num) {
      sgemm(opB, opA, M, N, K, ALPHA, BB, BO, N, AA, AO, K, BETA, C, CO, N)
      AO += M * K
      BO += K * N
      CO += M * N
      i += 1
    }
  }

  implicit class MatrixMultiplicationDoubleOps(a: Tensor[Double]) {

    def **(b: Tensor[Double]): Tensor[Double] = {
      val n = a.shape.length
      val outShape = a.shape.updated(n - 1, b.shape.last)
      val C = new Array[Double](outShape.product)
      val c = new ArrayTensor[Double](outShape, C, 0)
      dgemmT(a, b, c)
      c
    }
  }

  implicit class MatrixMultiplicationFloatOps(a: Tensor[Float]) {

    def **(b: Tensor[Float]): Tensor[Float] = {
      val n = a.shape.length
      val outShape = a.shape.updated(n - 1, b.shape.last)
      val C = new Array[Float](outShape.product)
      val c = new ArrayTensor[Float](outShape, C, 0)
      sgemmT(a, b, c)
      c
    }
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
