package com.github.arzt.tensor

import scala.collection.immutable
import scala.reflect.ClassTag

sealed trait Tensor[T] {

  def toDouble(implicit n: Numeric[T]): Tensor[Double] = this.map(x => n.toDouble(x))

  def shape: immutable.Seq[Int]

  require(shape.forall(_ > 0), "Tensor dimensions must be greater than zero")

  def rank: Int = shape.length

  val length: Int = shape.product

  private val stride: Array[Int] = shape
    .scanRight(1) { (right, size) =>
      right * size
    }
    .tail
    .toArray

  lazy val rows: Int = shape(rank - 2)

  lazy val cols: Int = shape.last

  def isVector: Boolean = shape.length == 1

  def isMatrix: Boolean = shape.length == 2

  def isView: Boolean

  def apply()(implicit tag: ClassTag[T]): Tensor[T] = new ArrayTensor[T](shape, toSeq.toArray, 0)

  def apply(a: Int): T

  def apply(b: Int, a: Int): T = apply(index(stride, b, a))

  def apply(c: Int, b: Int, a: Int): T = apply(index(stride, c, b, a))

  def apply(d: Int, c: Int, b: Int, a: Int): T = apply(index(stride, d, c, b, a))

  def update(a: Int, v: T): Unit // = throw new UnsupportedOperationException("Update not possible on view")

  def update(b: Int, a: Int, v: T): Unit = this(index(stride, b, a)) = v

  def update(c: Int, b: Int, a: Int, v: T): Unit = this(index(stride, c, b, a)) = v

  def apply(a: SeqIndex): Tensor[T] = {
    new ViewTensor(Vector(a.size), this, a.toArray)
  }

  def apply(b: SeqIndex, a: SeqIndex): Tensor[T] = {
    val mapping = indices2(stride, b.toArray, a.toArray)
    new ViewTensor(Vector(b.size, a.size), this, mapping)
  }

  def apply(c: SeqIndex, b: SeqIndex, a: SeqIndex): Tensor[T] = {
    val mapping = indices(stride, c.view, b.view, a.view).toArray
    new ViewTensor(Vector(c.size, b.size, a.size), this, mapping)
  }

  def apply(d: SeqIndex, c: SeqIndex, b: SeqIndex, a: SeqIndex): Tensor[T] = {
    val mapping = indices(stride, d.view, c.view, b.view, a.view)
    new ViewTensor[T](Vector(d.size, c.size, b.size, a.size), this, mapping)
  }

  def apply(a: Index): Tensor[T] = apply(a(shape(0)))

  def apply(a: Index, b: Index): Tensor[T] = {
    val i = shape(0)
    val u = a(i)
    val v = b(shape(1))
    apply(u, v)
  }

  def apply(a: Index, b: Index, c: Index): Tensor[T] = apply(a(shape(0)), b(shape(1)), c(shape(2)))

  def apply(a: Index, b: Index, c: Index, d: Index): Tensor[T] = apply(a(shape(0)), b(shape(1)), c(shape(2)), d(shape(3)))

  def update(that: Tensor[T]): Unit = {
    var i = 0
    while (i < length) {
      this(i) = that(i)
      i += 1
    }
  }

  def update(a: SeqIndex, that: Tensor[T]): Unit = apply(a)() = that

  def update(b: SeqIndex, a: SeqIndex, that: Tensor[T]): Unit = apply(b, a)() = that

  def update(c: SeqIndex, b: SeqIndex, a: SeqIndex, that: Tensor[T]): Unit = apply(c, b, a)() = that

  def :=(that: Tensor[T]): Unit = this() = that

  override def toString: String = {
    val split = this.map(_.toString.split('\n'))
    val maxHeight = split.toSeq.map(_.length).max
    val maxWidth = split.toSeq.map(_.map(_.length).max).max
    val string = split
      .map {
        x =>
          {
            val t = Tensor[Char](maxHeight, maxWidth + 1)
            val xs = x.toString.split('\n')
            val cs = Array.fill[Char]((maxWidth + 1) * maxWidth)(' ')
            val strRep = x.toString
            (0 until strRep.length)
              .foreach { i =>
                cs(i + 1) = strRep.charAt(i)
              }
            cs
          }
      }
      .apply()
    val tap = string.toSeq.sliding(cols, cols).map(x => x.reduce(_ ++ _) :+ '\n')
      .reduce(_ ++ _)
    new String(tap)
  }

  def combine[B, R](that: Tensor[B], f: (T, B) => R): Tensor[R] = new CombineTensor[R, T, B](this, that, f)

  def map[R](f: T => R): Tensor[R] = new MapTensor[T, R](this, f)

  def ==(a: T): Tensor[Boolean] = this.map(_ == a)

  def !=(a: T): Tensor[Boolean] = this.map(_ != a)

  def toSeq: Seq[T]

  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case obj: Tensor[T] => obj.shape.dropWhile(_ == 1) == this.shape.dropWhile(_ == 1) && obj.toSeq == this.toSeq
      case _ => false
    }
  }
}

object Tensor {

  def apply[T: ClassTag](dim: Int*): Tensor[T] = Tensor[T](dim.toVector)

  def apply[T: ClassTag](data: Array[T]): Tensor[T] =
    new ArrayTensor[T](Vector(data.length), data, 0)

  def apply[T: ClassTag](data: Array[T], offset: Int): Tensor[T] =
    new ArrayTensor[T](Vector(data.length), data, offset)

  def apply[T: ClassTag](shape: immutable.Seq[Int], data: Array[T], offset: Int): Tensor[T] =
    new ArrayTensor[T](shape, data, offset)

  def apply[T: ClassTag](shape: immutable.Seq[Int], data: Array[T]): Tensor[T] =
    new ArrayTensor[T](shape, data, 0)

  def apply[T: ClassTag](shape: immutable.Seq[Int]): Tensor[T] =
    new ArrayTensor[T](shape, new Array[T](shape.product), 0)

}

private class ArrayTensor[T] private[tensor] (
    val shape: immutable.Seq[Int],
    data: Array[T], offset: Int = 0) extends Tensor[T] {

  override def apply(a: Int): T = data(offset + a)

  override def update(a: Int, v: T): Unit = data(offset + a) = v

  override def apply()(implicit tag: ClassTag[T]): Tensor[T] = this

  override def toSeq: Seq[T] = data.view.slice(offset, length + offset)

  override def isView = false
}

private class CombineTensor[T, A, B](ta: Tensor[A], tb: Tensor[B], f: (A, B) => T) extends Tensor[T] {
  require(ta.shape == tb.shape)

  override def shape: immutable.Seq[Int] = ta.shape

  override def isView: Boolean = true

  override def apply(a: Int): T = f(ta(a), tb(a))

  override def toSeq: Seq[T] = (0 until ta.length).view.map(apply)

  override def update(a: Int, v: T): Unit =
    throw new UnsupportedOperationException("Update not supported on combined tensor view, call apply first")
}

private class MapTensor[T, R](tensor: Tensor[T], f: T => R) extends Tensor[R] {
  override def shape: immutable.Seq[Int] = tensor.shape

  override def apply()(implicit tag: ClassTag[R]): Tensor[R] = {
    val data = toSeq.toArray
    new ArrayTensor[R](shape, data, 0)
  }

  override def apply(a: Int): R = f(tensor(a))

  override def toSeq: Seq[R] = tensor.toSeq.map(f)

  override def isView = true

  override def update(a: Int, v: R): Unit =
    throw new UnsupportedOperationException("Update not supported on mapped tensor view, call apply first")
}

private class ViewTensor[T](val shape: immutable.Seq[Int], tensor: Tensor[T], map: Int => Int) extends Tensor[T] {

  override def apply(i: Int): T = tensor(map(i))

  override def update(i: Int, v: T): Unit = tensor.update(map(i), v)

  override def toSeq: collection.Seq[T] = (0 until length).view.map(apply)

  override def isView: Boolean = true

}
