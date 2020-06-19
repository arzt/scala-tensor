package com.github.arzt.math

import com.sun.jna.{ Library, Native, Pointer }

trait CBlasPointer extends Library {

  def cblas_dgemm(layout: Int, TransA: Int, TransB: Int, M: Int, N: Int, K: Int, alpha: Double, A: Pointer, lda: Int, B: Pointer, ldb: Int, beta: Double, C: Pointer, ldc: Int): Unit

}

object CBlasPointer {

  val INSTANCE: CBlasPointer = Native.load("cblas", classOf[CBlasPointer])

}
