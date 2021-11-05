package com.github.arzt.tensor

import com.github.arzt.tensor.TensorImplicits._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class OneHotSpec extends AnyFreeSpec with Matchers {

  "OneHot should" - {
    "represent one hot encoded tensor" in {
      val oneHot = OneHot(Vector(10), 1.0, 0.0, 4)
      val expected = Array[Double](0, 0, 0, 0, 1, 0, 0, 0, 0, 0).asRow
      oneHot === expected
    }
    "of double" in {
      OneHot.ofDouble(Vector(4), 0) * 2 === Array[Double](2, 0, 0, 0).asRow
    }
  }
}
