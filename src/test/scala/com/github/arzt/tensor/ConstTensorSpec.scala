package com.github.arzt.tensor

import com.github.arzt.tensor.TensorImplicits._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

private class ConstTensorSpec extends AnyFreeSpec with Matchers {
  "ConstTensor should" - {
    "repeat constant value" in {
      5.0.constTensor(3) === Array(5.0, 5.0, 5.0).asRow
    }
  }
}
