package com.github.arzt.tensor

import org.specs2.mutable.Specification
import com.github.arzt.tensor.TensorImplicits._

class OneHotSpec extends Specification {

  "OneHot" should {
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
