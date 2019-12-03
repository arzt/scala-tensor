package com.github.arzt.math

import org.specs2.mutable.Specification

class MathSpec extends Specification {
  "positive mod" should {
    "always compute positive modulus" in {
      val mod = positiveMod(4)(_)
      mod(3) === 3
      mod(5) === 1
      mod(-1) === 3
      mod(-4) === 0
      mod(-41) === 3
    }
  }
}
