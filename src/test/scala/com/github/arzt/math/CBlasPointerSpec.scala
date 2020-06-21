package com.github.arzt.math

import com.github.arzt.tensor.TensorImplicits._
import com.sun.jna.Memory
import org.specs2.mutable.Specification

class CBlasPointerSpec extends Specification {
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
    "dgemm col major" in {
      val A_ = A.asRows(3).t.toIterable.toArray
      val B_ = B.asRows(2).t.toIterable.toArray
      val C_ = C.asRows(3).t.toIterable.toArray

      val A_mem = new Memory(A_.length * 8)
      val B_mem = new Memory(B_.length * 8)
      val C_mem = new Memory(C_.length * 8)
      val D_mem = new Memory(C_.length * 8)
      A_mem.write(0, A_, 0, A_.length)
      B_mem.write(0, B_, 0, B_.length)
      C_mem.write(0, C_, 0, C_.length)

      val D = new Array[Double](M * N)
      CBlasPointer.INSTANCE.cblas_dgemm(
        layout = 102,
        TransA = 111,
        TransB = 111,
        M = M, N = N, K = K,
        A = A_mem, lda = M,
        B = B_mem, ldb = K,
        alpha = 1, beta = 0,
        C = D_mem, ldc = M)
      D_mem.read(0, D, 0, D.length)
      D.toSeq === C_.toSeq
    }
    "gemm row major pointer" in {
      val D = new Array[Double](M * N)
      val A_mem = new Memory(A.length * 8)
      val B_mem = new Memory(B.length * 8)
      val C_mem = new Memory(C.length * 8)
      val D_mem = new Memory(C.length * 8)
      A_mem.write(0, A, 0, A.length)
      B_mem.write(0, B, 0, B.length)
      C_mem.write(0, C, 0, C.length)

      A_mem.write(0, A, 0, A.length)
      CBlasPointer.INSTANCE.cblas_dgemm(
        layout = 102,
        TransA = 111,
        TransB = 111,
        M = N, N = M, K = K,
        A = B_mem, lda = N,
        B = A_mem, ldb = K,
        alpha = 1, beta = 0,
        C = D_mem, ldc = N)
      D_mem.read(0, D, 0, D.length)
      C.toSeq === D.toSeq
    }
  }
}
