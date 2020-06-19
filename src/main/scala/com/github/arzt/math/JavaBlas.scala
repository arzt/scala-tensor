package com.github.arzt.math

object JavaBlas {

  def dgemm(
    aOp: Char,
    bOp: Char,
    m: Int,
    n: Int,
    k: Int,
    a: Array[Double],
    ao: Int,
    b: Array[Double],
    bo: Int,
    c: Array[Double],
    co: Int): Unit = {

    var i = 0
    while (i < m) {
      var j = 0
      while (j < n) {
        val ci = co + i * m + j
        var l = 0
        while (l < k) {
          val il = i * k + l + ao
          val ilt = l * m + i + ao
          val lj = l * n + j + bo
          val ail = if (aOp == 't') a(ilt) else a(il)
          c(ci) += ail * b(lj)
          l += 1
        }
        j += 1
      }
      i += 1
    }
  }

  def igemm(
    m: Int,
    n: Int,
    k: Int,
    a: Array[Int],
    ao: Int,
    b: Array[Int],
    bo: Int,
    c: Array[Int],
    co: Int): Unit = {

    var i = 0
    while (i < m) {
      var j = 0
      while (j < n) {
        val ci = co + i * m + j
        var cv: Int = 0
        var l = 0
        while (l < k) {
          val il = i * k + l
          val lj = l * n + j
          cv += a(il) * b(lj)
          l += 1
        }
        c(ci) = cv
        j += 1
      }
      i += 1
    }
  }

}
