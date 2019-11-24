package com.github.arzt.tensor

import com.github.arzt.tensor.convert.Converter
import com.github.arzt.tensor.op.TensorMultiplication

import scala.collection.immutable
import scala.reflect.ClassTag

trait Tensor[T] {

  def toDouble(implicit n: Numeric[T]): Tensor[Double] = this.map(x => n.toDouble(x))

  def shape: immutable.Seq[Int]

  require(shape.forall(_ >= 0), "Tensor dimensions must be greater or equal to zero")

  def rank: Int = shape.length

  val length: Int = shape.product

  protected val stride: Array[Int] = toStride(shape.toArray)

  lazy val rows: Int = shape(rank - 2)

  lazy val cols: Int = shape.last

  def isVector: Boolean = shape.length == 1

  def isMatrix: Boolean = shape.length == 2

  def isView: Boolean

  def apply()(implicit tag: ClassTag[T]): Tensor[T] = new ArrayTensor[T](shape, toSeq.toArray, 0)

  def apply(a: Int): T

  def apply(c: Int, b: Int, a: Int): T = apply(index(stride, c, b, a))

  def apply(d: Int, c: Int, b: Int, a: Int): T = apply(index(stride, d, c, b, a))

  def update(a: Int, v: T): Unit // = throw new UnsupportedOperationException("Update not possible on view")

  def update(b: Int, a: Int, v: T): Unit = this(index(stride, b, a)) = v

  def update(c: Int, b: Int, a: Int, v: T): Unit = this(index(stride, c, b, a)) = v

  def apply(a: Index): Tensor[T] = {
    val sa = a(shape(0))
    new ViewTensor(Vector(sa.size), this, sa.toArray)
  }

  def apply(b: Index, a: Index): Tensor[T] = {
    val u = b(shape(0))
    val v = a(shape(1))
    val mapping = indices(stride, u, v)
    new ViewTensor(Vector(u.size, v.size), this, mapping)
  }

  def apply(c: Index, b: Index, a: Index): Tensor[T] = {
    val sa = c(shape(0))
    val sb = b(shape(1))
    val sc = a(shape(2))
    val mapping = indices(stride, sa, sb, sc)
    new ViewTensor(Vector(sa.length, sb.length, sc.length), this, mapping)
  }

  def apply(d: Index, c: Index, b: Index, a: Index): Tensor[T] = {
    val sd = d(shape(0))
    val sc = c(shape(1))
    val sb = b(shape(2))
    val sa = a(shape(3))
    val mapping = indices(stride, sd, sc, sb, sa)
    new ViewTensor(Vector(sd.length, sc.length, sb.length, sa.length), this, mapping)
  }

  def update(that: Tensor[T]): Unit = {
    var i = 0
    while (i < length) {
      this(i) = that(i)
      i += 1
    }
  }

  def update(a: Index, that: Tensor[T]): Unit = apply(a)() = that

  def update(b: Index, a: Index, that: Tensor[T]): Unit = apply(b, a)() = that

  def update(c: Index, b: Index, a: Index, that: Tensor[T]): Unit = apply(c, b, a)() = that

  def :=(that: Tensor[T]): Unit = this() = that

  override def toString: String = {
    /*
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
    * */
    val shapeStr = shape.mkString("[", " ", "]")
    val valuesStr = toSeq.take(30).mkString("[", " ", "]")
    s"Tensor(n=$length, shape=$shapeStr, values=$valuesStr)"
  }

  def combine[B, R: ClassTag](that: Tensor[B], f: (T, B) => R): Tensor[R] = new CombineTensor[R, T, B](this, that, f)

  def map[R: ClassTag](f: T => R): Tensor[R] = new MapTensor[T, R](this, f)

  def ==(a: T): Tensor[Boolean] = this.map(_ == a)

  def !=(a: T): Tensor[Boolean] = this.map(_ != a)

  def toSeq: Seq[T]

  def **(b: Tensor[T])(implicit m: TensorMultiplication[T]): Tensor[T] = {
    import m.tag
    val n = this.shape.length
    val outShape = this.shape.updated(n - 1, b.shape.last)
    val C = new Array[T](outShape.product)
    val c = new ArrayTensor[T](outShape, C, 0)
    m.apply(this, b, c)
    c
  }

  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case obj: Tensor[T] => obj.shape.dropWhile(_ == 1) == this.shape.dropWhile(_ == 1) && obj.toSeq == this.toSeq
      case _ => false
    }
  }

  def permute(pi: Int => Int): Tensor[T] = {
    val newShape = shape.indices.map(pi andThen shape)
    val mapping = permuteMapping(pi, stride, newShape)
    new ViewTensor[T](newShape, this, mapping)
  }

  def t: Tensor[T] = this match {
    case x: TransposeTensor[T] =>
      x.tensor
    case _ => {
      val n = shape.length - 1
      val pi = shape.indices.toArray
      val tmp = pi(n)
      pi(n) = pi(n - 1)
      pi(n - 1) = tmp
      val newShape = shape.indices.map(pi andThen shape)
      val mapping = permuteMapping(pi, stride, newShape)
      new TransposeTensor[T](newShape, this, mapping)
    }
  }

  private[tensor] def getData(implicit tag: ClassTag[T]): Array[T] =
    this match {
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
        this()
          .asInstanceOf[ArrayTensor[T]]
          .data
    }

  def reshape(newShape: Int*): Tensor[T] = {
    require(this.length == newShape.product, "Reshaping with incompatible shapes.")
    new ReshapeTensor[T](newShape.toVector, this)
  }

  def asRow(): Tensor[T] = reshape(1, this.length)

  def asCol(): Tensor[T] = reshape(this.length, 1)

  def dropSingular(dim: Int): Tensor[T] = {
    require(shape(dim) == 1, "dimension at index $dim must be equal 1")
    val newShape = shape
      .indices
      .view
      .filter(_ != dim)
      .map(shape)
      .toIndexedSeq
    new ReshapeTensor[T](newShape, this)
  }

  def addSingular(dim: Int): Tensor[T] = {
    val before = shape.slice(0, dim)
    val after = shape.view.slice(dim, shape.length)
    val newShape = (before :+ 1) ++ after
    new ReshapeTensor[T](newShape.toIndexedSeq, this)
  }

  def dissect(dims: Int*): Tensor[Tensor[T]] = {
    val remainingDims = shape.indices.toSet -- dims
    val parentShape = dims
      .foldLeft(shape)(_.updated(_, 1))
    val childShape = remainingDims
      .foldLeft(shape)(_.updated(_, 1))
    new IndexTensor(parentShape)
      .map { index =>
        val is = new Array[Seq[Int]](shape.length)
        remainingDims
          .foreach { i =>
            is(i) = Seq(index(i))
          }
        dims
          .foreach { i =>
            is(i) = 0 until shape(i)
          }
        val mapping = indices(stride, is.toSeq: _*)
        new ViewTensor(childShape, this, mapping): Tensor[T]
      }
      .apply()
  }

  def inflate[U: ClassTag](implicit converter: Converter[T, U]): Tensor[U] = {
    val newShape = shape.updated(shape.indices.last, shape.last * converter.n)
    new InflateTensor[U, T](newShape, this)
  }

  def deflate[U: ClassTag](implicit converter: Converter[U, T]): Tensor[U] = {
    val d = shape.last
    if (d % converter.n == 0) {
      val newShape = shape.updated(shape.indices.last, d / converter.n)
      new DeflateTensor[U, T](newShape, this)
    } else {
      throw new IllegalArgumentException(s"Last shape dimension is not divisible by ${converter.n}")
    }
  }

}

object Tensor {

  def apply[T: ClassTag](shape: Int*): Tensor[T] =
    new ArrayTensor[T](shape.toVector, new Array[T](shape.product), 0)

  def apply[T: ClassTag](data: Array[T], shape: Int*): Tensor[T] = {
    new ArrayTensor[T](if (shape.isEmpty) Vector(data.length) else shape.toVector, data, 0)
  }

  def apply[T: ClassTag](offset: Int, data: Array[T]): Tensor[T] =
    new ArrayTensor[T](Vector(data.length), data, offset)

  def apply[T: ClassTag](offset: Int, data: Array[T], shape: Int*): Tensor[T] =
    new ArrayTensor[T](shape.toVector, data, offset)

}

private class ArrayTensor[T] private[tensor] (
    val shape: immutable.Seq[Int],
    val data: Array[T],
    val offset: Int = 0)(implicit val tag: ClassTag[T]) extends Tensor[T] {

  override def apply(a: Int): T = data(offset + a)

  override def update(a: Int, v: T): Unit = data(offset + a) = v

  override def apply()(implicit tag: ClassTag[T]): Tensor[T] = this

  override def toSeq: Seq[T] = data.slice(offset, length + offset).toSeq

  override def isView = false
}

private class CombineTensor[T, A, B](ta: Tensor[A], tb: Tensor[B], f: (A, B) => T)(implicit val tag: ClassTag[T]) extends Tensor[T] {
  require(ta.shape == tb.shape)

  override def shape: immutable.Seq[Int] = ta.shape

  override def isView: Boolean = true

  override def apply(a: Int): T = f(ta(a), tb(a))

  override def toSeq: Seq[T] = (0 until ta.length).view.map(apply).toArray.toSeq

  override def update(a: Int, v: T): Unit =
    throw new UnsupportedOperationException("Update not supported on combined tensor view, call apply first")
}

private class MapTensor[T, R](tensor: Tensor[T], f: T => R)(implicit val tag: ClassTag[R]) extends Tensor[R] {
  override def shape: immutable.Seq[Int] = tensor.shape

  override def apply(a: Int): R = f(tensor(a))

  override def toSeq: Seq[R] = tensor.toSeq.map(f)

  override def isView = true

  override def update(a: Int, v: R): Unit =
    throw new UnsupportedOperationException("Update not supported on mapped tensor view, call apply first")
}

private class ViewTensor[T](val shape: immutable.Seq[Int], val tensor: Tensor[T], map: Int => Int) extends Tensor[T] {

  override def apply(i: Int): T = tensor(map(i))

  override def update(i: Int, v: T): Unit = tensor.update(map(i), v)

  override def toSeq: Seq[T] = (0 until length).map(apply)

  override def isView: Boolean = true

}

private class TransposeTensor[T](shape: immutable.Seq[Int], tensor: Tensor[T], map: Int => Int)
  extends ViewTensor[T](shape, tensor, map)

private class ReshapeTensor[T](val shape: immutable.Seq[Int], val tensor: Tensor[T]) extends Tensor[T] {
  override def isView: Boolean = true

  override def apply(i: Int): T = tensor(i)

  override def update(i: Int, v: T): Unit = tensor(i) = v

  override def toSeq: Seq[T] = (0 until length).map(apply)
}

class EchoTensor(val shape: immutable.Seq[Int]) extends Tensor[Int] {

  override def isView: Boolean = true

  override def apply(a: Int): Int = a

  override def update(a: Int, v: Int): Unit =
    throw new UnsupportedOperationException("Update not supported on IndexTensor, call apply() first")

  override def toSeq: Seq[Int] = 0 until length
}

object EchoTensor {
  def apply(shape: immutable.Seq[Int]): Tensor[Int] = new EchoTensor(shape)
}

private class IndexTensor(val shape: immutable.Seq[Int]) extends Tensor[Seq[Int]] {

  override def isView: Boolean = false

  override def apply(i: Int): Seq[Int] = {
    val output = new Array[Int](shape.length)
    unindex(stride, output)(i)
    output.toSeq
  }

  override def update(a: Int, v: Seq[Int]): Unit =
    throw new UnsupportedOperationException("Update not supported on IndexTensor, call apply() first")

  override def toSeq: Seq[Seq[Int]] = (0 until length).map(apply)
}

object IndexTensor {
  def apply(shape: immutable.Seq[Int]): Tensor[Seq[Int]] = new IndexTensor(shape)
}

