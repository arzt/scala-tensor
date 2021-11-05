package com.github.arzt.tensor

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class DiagTensorSpec extends AnyFreeSpec with Matchers {
  "diag tensor should" - {
    "work" in {
      val tensor = Tensor(0, Array[Double](1, 2, 3)).diag(0)
      tensor.sameElements(Array[Double](
        1, 0, 0,
        0, 2, 0,
        0, 0, 3))
    }
    "work2" in {
      val tensor = Tensor(0, Array[Double](1, 2, 3, 4))
        .reshape(2, 2)
        .diag(0)
      tensor.sameElements(Array[Double](
        1, 0,
        0, 2,
        3, 0,
        0, 4))
      tensor.shape === Vector(2, 2, 2)
    }
    "mapping" in {
      DiagTensor.mapping(3)(0) === 0
      DiagTensor.mapping(3)(1) === -1
      DiagTensor.mapping(3)(4) === 1
      DiagTensor.mapping(3)(8) === 2
      DiagTensor.mapping(3)(7) === -1
    }
  }
}
