package com.github.arzt.tensor.op

import com.github.arzt.tensor.ArrayTensor
import com.github.arzt.tensor.MyBLAS
import com.github.arzt.tensor.Tensor
import com.github.arzt.tensor.TensorImplicits.getOffset
import com.github.arzt.tensor.TensorImplicits.getOp
import org.jblas.NativeBlas.dgemm
import org.jblas.NativeBlas.sgemm

trait TensorMultiplication[T] {

  def apply(a: Tensor[T], b: Tensor[T], c: ArrayTensor[T]): Unit
}

object JavaDoubleTensorMultiplication extends TensorMultiplication[Double] {
  override def apply(a: Tensor[Double], b: Tensor[Double], c: ArrayTensor[Double]): Unit = {
    val n = a.shape.length
    require(n == b.shape.length)
    require(a.shape(n - 1) == b.shape(n - 2))
    val A = a.getData
    val B = b.getData
    val opA = getOp(a)
    val opB = getOp(b)
    val num = a.shape.take(n - 2).product
    val C = c.getData
    val M = a.shape(n - 2)
    val N = b.shape.last
    val K = a.shape.last
    val ALPHA = 1
    val BETA = 0
    var AO = getOffset(a)
    var BO = getOffset(b)
    var CO = c.offset
    var i = 0
    while (i < num) {
      MyBLAS.dgemmJava(opA, opB, M, N, K, A, AO, B, BO, C, CO)
      AO += M * K
      BO += K * N
      CO += M * N
      i += 1
    }
  }
}

object DoubleTensorMultiplication extends TensorMultiplication[Double] {

  override def apply(a: Tensor[Double], b: Tensor[Double], c: ArrayTensor[Double]): Unit = {
    val n = a.shape.length
    require(n == b.shape.length)
    require(a.shape(n - 1) == b.shape(n - 2))
    val A = a.getData
    val B = b.getData
    val opA = getOp(a)
    val opB = getOp(b)
    val num = a.shape.take(n - 2).product
    val C = c.getData
    val M = a.shape(n - 2)
    val N = b.shape.last
    val K = a.shape.last
    val ALPHA = 1
    val BETA = 0
    var AO = getOffset(a)
    var BO = getOffset(b)
    var CO = c.offset
    var i = 0
    while (i < num) {
      dgemm(opB, opA, M, N, K, ALPHA, B, BO, N, A, AO, K, BETA, C, CO, N)
      AO += M * K
      BO += K * N
      CO += M * N
      i += 1
    }
  }

}

object FloatTensorMultiplication extends TensorMultiplication[Float] {
  override def apply(a: Tensor[Float], b: Tensor[Float], c: ArrayTensor[Float]): Unit = {
    val n = a.shape.length
    require(n == b.shape.length)
    require(a.shape(n - 1) == b.shape(n - 2))
    val A = a.getData
    val B = b.getData
    val opA = getOp(a)
    val opB = getOp(b)
    val num = a.shape.take(n - 2).product
    val C = c.getData
    val M = a.shape(n - 2)
    val N = b.shape.last
    val K = a.shape.last
    val ALPHA = 1
    val BETA = 0
    var AO = getOffset(a)
    var BO = getOffset(b)
    var CO = c.offset
    var i = 0
    while (i < num) {
      sgemm(opB, opA, M, N, K, ALPHA, B, BO, N, A, AO, K, BETA, C, CO, N)
      AO += M * K
      BO += K * N
      CO += M * N
      i += 1
    }
  }
}