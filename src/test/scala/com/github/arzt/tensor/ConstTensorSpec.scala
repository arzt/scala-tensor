package com.github.arzt.tensor

import org.specs2.mutable.Specification
import TensorImplicits._

private class ConstTensorSpec extends Specification {
  "ConstTensor" should {
    "repeat constant value" in {
      5.0.constTensor(3) === Array(5.0, 5.0, 5.0).asRow
    }
  }
}
