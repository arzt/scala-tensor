package com.github.arzt.tensor

import java.util.function.IntConsumer
import java.util.stream.IntStream

import com.github.arzt.tensor.convert.Converter
import com.github.arzt.tensor.dim.Dim
import com.github.arzt.tensor.op.TensorMultiplication

import scala.collection.compat.immutable.ArraySeq.unsafeWrapArray
import scala.collection.immutable.Seq
import scala.reflect.ClassTag
import scala.util.Random

trait Tensor[T] {

  def shape: Seq[Int]

  require(shape.forall(_ >= 0), "Tensor dimensions must be greater or equal to zero")

  val length: Int = shape.product

  protected val stride: Array[Int] = toStride(shape.toArray)

  lazy val rows: Int = shape(shape.length - 2)

  lazy val cols: Int = shape.last

  def isVector: Boolean = shape.length == 1

  def isMatrix: Boolean = shape.length == 2

  def isView: Boolean

  def cached(implicit tag: ClassTag[T]): Tensor[T] =
    cached(new Array[T](length))

  def cached(buf: Array[T], offset: Int = 0): Tensor[T] = {
    var i = 0
    while (i < length) {
      buf(i + offset) = this(i)
      i += 1
    }
    new ArrayTensor[T](shape, buf, offset)
  }

  def cachedParallel(buf: Array[T], offset: Int = 0): ArrayTensor[T] = {
    IntStream.range(0, length).parallel()
      .forEach(
        new IntConsumer {
          override def accept(i: Int): Unit = {
            buf(i + offset) = apply(i)
          }
        })
    new ArrayTensor[T](shape, buf, offset)
  }

  def cachedParallel(implicit tag: ClassTag[T]): Tensor[T] =
    cached(new Array[T](length))

  def apply(a: Int): T

  def update(a: Int, v: T): Unit // = throw new UnsupportedOperationException("Update not possible on view")

  def apply(is: Index*): Tensor[T] = {
    val list = shape
      .indices
      .map(
        i => {
          val dim = shape(i)
          val index = is(i)
          (dim, unsafeWrapArray(index(dim).toArray): Seq[Int])
        })
      .toList
    val mapping = Dim(list)
    new ViewTensor(mapping.shape, this, mapping)
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

  def combine[B, R](that: Tensor[B], f: (T, B) => R): Tensor[R] = new CombineTensor[R, T, B](this, that, f)

  def map[R](f: T => R): Tensor[R] = new MapTensor[T, R](this, f)

  def ==(a: T): Tensor[Boolean] = this.map(_ == a)

  def !=(a: T): Tensor[Boolean] = this.map(_ != a)

  def toIterable: Iterable[T] = (0 until length).view.map(this.apply)

  def sameElements(that: Iterable[T]): Boolean =
    this.toIterable.iterator.sameElements(that.iterator)

  def **(b: Tensor[T])(implicit m: TensorMultiplication[T]): Tensor[T] = {
    import m.tag
    val n = this.shape.length
    val outShape = this.shape.updated(n - 1, b.shape.last)
    val C = new Array[T](outShape.product)
    val c = new ArrayTensor[T](outShape, C, 0)
    m.apply(this, b, c)
    c
  }

  override def equals(that: scala.Any): Boolean = {
    that match {
      case tensor: Tensor[T] =>
        val compatibleShape = tensor.shape.filter(_ != 1) == this.shape.filter(_ != 1)
        val sameElements = tensor.sameElements(this.toIterable)
        compatibleShape && sameElements
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

  def getData(implicit tag: ClassTag[T]): Array[T] =
    this match {
      case array: ArrayTensor[T] =>
        array.getData
      case transpose: TransposeTensor[T] =>
        transpose.tensor match {
          case array2: ArrayTensor[T] =>
            array2.getData
          case _ =>
            transpose
              .cached
              .getData
        }
      case _ =>
        this
          .cached
          .getData
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
        remainingDims.foreach { i =>
          is(i) = index(i) to index(i)
        }
        dims.foreach { i =>
          is(i) = 0 until shape(i)
        }
        val mapping = Dim(shape.zip(is).toList)
        new ViewTensor(childShape, this, mapping): Tensor[T]
      }
  }

  def inflate[U](implicit converter: Converter[T, U]): Tensor[U] = {
    val newShape = shape.updated(shape.indices.last, shape.last * converter.n)
    new InflateTensor[U, T](newShape, this)
  }

  def deflate[U](implicit converter: Converter[U, T]): Tensor[U] = {
    val d = shape.last
    if (d % converter.n == 0) {
      val newShape = shape.updated(shape.indices.last, d / converter.n)
      new DeflateTensor[U, T](newShape, this)
    } else {
      throw new IllegalArgumentException(s"Last shape dimension is not divisible by ${converter.n}")
    }
  }

  override def toString: String = toIterable.take(200).mkString("Tensor(", ",", "...)")

  def diag(default: T): Tensor[T] = new DiagTensor[T](
    shape = shape :+ shape.last,
    tensor = this,
    default = default)

  def withRelaxedShape(length: Int): Tensor[T] = {
    val newShapeIt = Iterator.fill(length - shape.length)(1) ++ shape
    this.reshape(newShapeIt.toSeq: _*)
  }
}

object Tensor {

  def apply[T: ClassTag](shape: Int*): Tensor[T] =
    new ArrayTensor[T](shape.toVector, new Array[T](shape.product), 0)

  def apply[T](data: Array[T], shape: Int*): Tensor[T] = {
    new ArrayTensor[T](if (shape.isEmpty) Vector(data.length) else shape.toVector, data, 0)
  }

  def apply[T](offset: Int, data: Array[T]): Tensor[T] =
    new ArrayTensor[T](Vector(data.length), data, offset)

  def apply[T](offset: Int, data: Array[T], shape: Int*): Tensor[T] =
    new ArrayTensor[T](shape.toVector, data, offset)

}

class ArrayTensor[T] private[tensor] (
    val shape: Seq[Int],
    private val data: Array[T],
    val offset: Int = 0) extends Tensor[T] {

  override def apply(a: Int): T = data(offset + a)

  override def update(a: Int, v: T): Unit = data(offset + a) = v

  override def isView = false

  override def cached(implicit tag: ClassTag[T]): ArrayTensor[T] = this

  override def getData(implicit tag: ClassTag[T]): Array[T] = data

}

private class CombineTensor[T, A, B](ta: Tensor[A], tb: Tensor[B], f: (A, B) => T) extends Tensor[T] {
  require(ta.shape == tb.shape)

  override def shape: Seq[Int] = ta.shape

  override def isView: Boolean = true

  override def apply(a: Int): T = f(ta(a), tb(a))

  override def update(a: Int, v: T): Unit =
    throw new UnsupportedOperationException("Update not supported on combined tensor view, call apply first")
}

private class MapTensor[T, R](tensor: Tensor[T], f: T => R) extends Tensor[R] {
  override def shape: Seq[Int] = tensor.shape

  override def apply(a: Int): R = f(tensor(a))

  override def isView = true

  override def update(a: Int, v: R): Unit =
    throw new UnsupportedOperationException("Update not supported on mapped tensor view, call apply first")
}

private class ViewTensor[T](val shape: Seq[Int], val tensor: Tensor[T], map: Int => Int) extends Tensor[T] {

  override def apply(i: Int): T = tensor(map(i))

  override def update(i: Int, v: T): Unit = tensor.update(map(i), v)

  override def isView: Boolean = true

}

private class TransposeTensor[T](shape: Seq[Int], tensor: Tensor[T], map: Int => Int)
  extends ViewTensor[T](shape, tensor, map)

private class ReshapeTensor[T](val shape: Seq[Int], val tensor: Tensor[T]) extends Tensor[T] {
  override def isView: Boolean = true

  override def apply(i: Int): T = tensor(i)

  override def update(i: Int, v: T): Unit = tensor(i) = v

}

class EchoTensor(val shape: Seq[Int]) extends Tensor[Int] {

  override def isView: Boolean = true

  override def apply(a: Int): Int = a

  override def update(a: Int, v: Int): Unit =
    throw new UnsupportedOperationException("Update not supported on IndexTensor, call apply() first")

}

object EchoTensor {
  def apply(shape: Seq[Int]): Tensor[Int] = new EchoTensor(shape)
}

private class IndexTensor(val shape: Seq[Int]) extends Tensor[Seq[Int]] {

  override def isView: Boolean = false

  override def apply(i: Int): Seq[Int] = {
    val output = new Array[Int](shape.length)
    unindex(stride, output)(i)
    unsafeWrapArray(output)
  }

  override def update(a: Int, v: Seq[Int]): Unit =
    throw new UnsupportedOperationException("Update not supported on IndexTensor, call apply() first")

}

object IndexTensor {
  def apply(shape: Seq[Int]): Tensor[Seq[Int]] = new IndexTensor(shape)
}

object Rand {
  def apply(shape: Int*): Tensor[Double] = {
    val r = new Random()
    EchoTensor(shape.toIndexedSeq).map(_ => r.nextDouble())
  }
}

object Randn {
  def apply(shape: Int*): Tensor[Double] = {
    val r = new Random()
    EchoTensor(shape.toIndexedSeq).map(_ => r.nextGaussian())
  }
}