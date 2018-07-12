package com.github.arzt.tensor

import org.specs2.mutable.Specification

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
      MyBLAS.dgemmJava(m, n, k, a, ao, b, bo, c, co)
      1 === 1
    }
  }

}
