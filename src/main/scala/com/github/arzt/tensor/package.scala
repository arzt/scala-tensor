package com.github.arzt

package object tensor {
  type Index = Int => Seq[Int]

  def indices(stride: Array[Int], d: Array[Int], c: Array[Int], b: Array[Int], a: Array[Int]): Array[Int] = {
    val n = d.length * c.length * b.length * a.length
    val output = new Array[Int](n)
    var id = 0
    var i = 0
    while (id < d.length) {
      val jd = stride(0) * d(id)
      var ic = 0
      while (ic < c.length) {
        val jc = jd + stride(1) * c(ic)
        var ib = 0
        while (ib < b.length) {
          val jb = jc + stride(2) * b(ib)
          var ia = 0
          while (ia < a.length) {
            output(i) = jb + a(ia)
            ia += 1
            i += 1
          }
          ib += 1
        }
        ic += 1
      }
      id += 1
    }
    output
  }

  def indices(stride: Array[Int], c: Array[Int], b: Array[Int], a: Array[Int]): Array[Int] = {
    val n = c.length * b.length * a.length
    val output = new Array[Int](n)
    var ic = 0
    var i = 0
    while (ic < c.length) {
      val j = stride(0) * c(ic)
      var ib = 0
      while (ib < b.length) {
        val k = j + stride(1) * b(ib)
        var ia = 0
        while (ia < a.length) {
          output(i) = k + a(ia)
          ia += 1
          i += 1
        }
        ib += 1
      }
      ic += 1
    }
    output
  }

  def indices(stride: Array[Int], b: Array[Int], a: Array[Int]): Array[Int] = {
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
