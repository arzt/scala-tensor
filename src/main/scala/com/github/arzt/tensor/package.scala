package com.github.arzt

import scala.collection.mutable

package object tensor {
  type Index = Int => Seq[Int]
  type SeqIndex = Seq[Int]

  def indices(stride: Array[Int], d: Seq[Int], c: Seq[Int], b: Seq[Int], a: Seq[Int]): Seq[Int] =
    for (
      di <- d;
      h = stride(0) * di;
      ci <- c;
      i = stride(1) * ci + h;
      bi <- b;
      j = stride(2) * bi + i;
      ai <- a
    ) yield {
      j + ai
    }

  def indices(stride: Array[Int], c: Seq[Int], b: Seq[Int], a: Seq[Int]): Seq[Int] = {
    for (
      ci <- c;
      i = stride(0) * ci;
      bi <- b;
      j = stride(1) * bi + i;
      ai <- a
    ) yield {
      j + ai
    }
  }

  def indices(stride: Array[Int], b: Array[Int], a: Array[Int]): Array[Int] = {
    val wb = mutable.WrappedArray.make[Int](b)
    val wa = mutable.WrappedArray.make[Int](a)
    (for (
      bi <- wb;
      j = stride(0) * bi;
      ai <- wa
    ) yield {
      j + ai
    }).toArray
  }

  def indices2(stride: Array[Int], b: Array[Int], a: Array[Int]): Array[Int] = {
    val n = b.length * a.length
    val output = new Array[Int](n)
    var ib = 0
    var i = 0
    while (ib < b.length) {
      val j = stride(0) * b(ib)
      var ia = 0
      while (ia < a.length) {
        output(i) = a(ia) + j
        ia += 1
        i += 1
      }
      ib += 1
    }
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
}
