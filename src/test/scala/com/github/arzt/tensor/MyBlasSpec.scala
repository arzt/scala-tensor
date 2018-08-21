package com.github.arzt.tensor

import org.specs2.mutable.Specification
import TensorImplicits._
import com.github.arzt.tensor.op.JavaDoubleTensorMultiplication

class MyBlasSpec extends Specification {

  "dgemm" should {
    "compute matrix mul" in {
      val m = 2
      val n = 2
      val k = 3
      val a =
        Array[Double](1, 2, 3,
          4, 5, 6)
      val ao = 0
      val b =
        Array[Double](1, 2,
          3, 4,
          5, 6)
      val bo = 0
      val c = new Array[Double](m * n)
      val co = 0

      val at = a.asTensor(2, 3)
      val bt = b.asTensor(3, 2)

      val ct = at ** bt

      val dt = (at ** bt)(JavaDoubleTensorMultiplication)

      MyBLAS.dgemmJava('n', 'n', m, n, k, a, ao, b, bo, c, co)
      val c2t = c.asTensor(2, 2)
      ct === c2t
    }
  }

}
