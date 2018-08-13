package com.github.arzt.tensor

import org.jblas.NativeBlas
import org.specs2.mutable.Specification

class BlasSpec extends Specification {
  "jBLAS" should {
    "multiply with offset" in {
      val A = Array[Double](
        1, 0, 0,
        0, 5, 0,
        0, 0, 3)
      val B = Array[Double](
        4, 0, 0,
        0, 4, 0,
        0, 0, 2)
      val C = Array[Double](
        0,
        0, 0, 0,
        0, 0, 0,
        0, 0, 0)
      val M = 3
      val N = 3
      val K = 3
      val ALPHA = 1
      val LDA = K
      val LDB = N
      val LDC = K
      val BETA = 0
      NativeBlas.dgemm('n', 'n', M, N, K, ALPHA, A, 0, LDA, B, 0, LDB, BETA, C, 1, LDC)
      C(1) === 4.0
      C(5) === 20.0
      C(9) === 6.0
    }
    "multiply 3x2" in {
      val A = Array[Double](
        1, 0, 0,
        0, 5, 0)
      val B = Array[Double](
        4, 0,
        0, 4,
        0, 0)
      val C = Array[Double](
        0,
        0, 0, 0,
        0, 0, 0,
        0, 0, 0)
      val M = 3
      val N = 3
      val K = 3
      val ALPHA = 1
      val LDA = M
      val LDB = N
      val LDC = K
      val BETA = 0
      NativeBlas.dgemm('n', 'n', M, N, K, ALPHA, A, 0, LDA, B, 0, LDB, BETA, C, 1, LDC)
      C(1) === 4.0
      C(5) === 20.0
      C(9) === 6.0
    }
  }
}
