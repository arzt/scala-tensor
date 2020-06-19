package com.github.arzt.math

import com.sun.jna.Library
import com.sun.jna.Native

trait CBlas extends Library {

  def cblas_dgemm(layout: Int, TransA: Int, TransB: Int, M: Int, N: Int, K: Int, alpha: Double, A: Array[Double], lda: Int, B: Array[Double], ldb: Int, beta: Double, C: Array[Double], ldc: Int): Unit

}

object CBlas {

  val INSTANCE: CBlas = Native.load("cblas", classOf[CBlas])

  def cblas_dgemm(layout: Int, TransA: Int, TransB: Int, M: Int, N: Int, K: Int, alpha: Double, A: Array[Double], lda: Int, B: Array[Double], ldb: Int, beta: Double, C: Array[Double], ldc: Int): Unit =
    INSTANCE.cblas_dgemm(layout, TransA, TransB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc)
}
