package com.github.arzt

import collection.immutable.Seq

package object tensor {
  type Index = Int => Iterable[Int]

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

  private[tensor] def toStride(dim: collection.Seq[Int]): Array[Int] = dim.scanRight(1)(_ * _).tail.toArray

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
