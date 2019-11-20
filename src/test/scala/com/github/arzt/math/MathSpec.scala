package com.github.arzt.math

import org.specs2.mutable.Specification

class MathSpec extends Specification {
  "generalized mod" should {
    "extenda mod for negative numbers" in {
      generalizedMod(3, 4) === 3
      generalizedMod(5, 4) === 1
      generalizedMod(-1, 4) === 3
      generalizedMod(-4, 4) === 0
    }
  }
}
