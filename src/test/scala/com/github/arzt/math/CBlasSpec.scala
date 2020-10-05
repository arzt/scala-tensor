package com.github.arzt.math

import com.github.arzt.tensor.TensorImplicits._
import org.specs2.mutable.Specification

class CBlasSpec extends Specification {
  "CBlas" should {
    val M = 3
    val N = 4
    val K = 2
    val A = Array[Double](
      -2, 1,
      0, 2,
      -3, 2)
    val B = Array[Double](
      2, -1, 1, 2,
      -1, 0, -1, 2)
    val C = Array[Double](
      -5, 2, -3, -2,
      -2, 0, -2, 4,
      -8, 3, -5, -2)
    "dgemm col major" in skipped {
      val A_ = A.asRows(3).t.toIterable.toArray
      val B_ = B.asRows(2).t.toIterable.toArray
      val C_ = C.asRows(3).t.toIterable.toArray
      val D = new Array[Double](M * N)
      CBlas.INSTANCE.cblas_dgemm(
        layout = 102,
        TransA = 111,
        TransB = 111,
        M = M, N = N, K = K,
        A = A_, lda = M,
        B = B_, ldb = K,
        alpha = 1, beta = 0,
        C = D, ldc = M)
      D.toSeq === C_.toSeq
    }
  }
}
