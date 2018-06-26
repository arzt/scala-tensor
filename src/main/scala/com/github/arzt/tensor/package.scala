package com.github.arzt

package object tensor {
  type Index = Int => Seq[Int]

  def indices(stride: Array[Int], is: Array[Int]*): Array[Int] = {
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
}
