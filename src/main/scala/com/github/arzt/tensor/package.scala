package com.github.arzt

package object tensor {
  type Index = Int => collection.Seq[Int]

  def getIndices(stride: Array[Int], is: collection.Seq[Int]*): Array[Int] = {
    val n = is.view.map(_.length).product
    val output = new Array[Int](n)
    var i = 0

    def indicesRec(l: Int, m: Int, offset: Int): Unit =
      if (m == is.length) {
        output(i) = offset
        i += 1
      } else {
        var k = 0
        while (k < is(m).length) {
          val newOffset = offset + stride(l) * is(m)(k)
          indicesRec(l + 1, m + 1, newOffset)
          k += 1
        }
      }

    indicesRec(0, 0, 0)
    output
  }

  def index(stride: Array[Int], b: Int, a: Int): Int = {
    b * stride(0) + a
  }

  def index(stride: Array[Int], c: Int, b: Int, a: Int): Int = {
    c * stride(0) + b * stride(1) + a
  }

  def index(stride: Array[Int], d: Int, c: Int, b: Int, a: Int): Int = {
    d * stride(0) + c * stride(1) + b * stride(2) + a
  }

  private[tensor] def index(stride: Array[Int]): (Int => Int) => Int =
    is => {
      var i = 0
      var out: Int = 0
      while (i < stride.length) {
        val isi = is(i)
        out += isi * stride(i)
        i += 1
      }
      out
    }

  private[tensor] def unindex(stride: Array[Int], output: Array[Int]): Int => Int => Int = {
    def unindexRec(i: Int, rest: Int): Int =
      if (i < stride.length) {
        val nextRest = rest % stride(i)
        output(i) = rest / stride(i)
        unindexRec(i + 1, nextRest)
      } else {
        rest
      }

    in => {
      unindexRec(0, in)
      output
    }
  }

  private[tensor] def toStride(dim: Array[Int]): Array[Int] = dim.scanRight(1)(_ * _).tail

  private[tensor] def invert(size: Int, fun: Int => Int): Int => Int = {
    val inverse = new Array[Int](size)
    inverse.indices.foreach { i =>
      inverse(fun(i)) = i
    }
    inverse
  }

  private[tensor] def permuteMapping(
    pi: Int => Int,
    fromStride: Array[Int],
    newShape: Seq[Int]): Int => Int = {
    val newStride = toStride(newShape.toArray)
    val output = new Array[Int](newShape.length)
    val unind = unindex(newStride, output)
    val ind = index(fromStride)
    val invPi = invert(fromStride.length, pi)
    unind andThen (invPi andThen _) andThen ind
  }
}
