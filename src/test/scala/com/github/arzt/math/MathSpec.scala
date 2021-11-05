package com.github.arzt.math

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class MathSpec extends AnyFreeSpec with Matchers {
  "positive mod should" - {
    "always compute positive modulus" in {
      val mod = positiveMod(4)(_)
      mod(3) === 3
      mod(5) === 1
      mod(-1) === 3
      mod(-4) === 0
      mod(-41) === 3
    }
  }
  "unsigned should" - {
    "compute unsigned value as int" in {
      asUnsignedInteger(8)(0) === 0
      asUnsignedInteger(8)(100) === 100
      asUnsignedInteger(8)(127) === 127
      asUnsignedInteger(8)(-128) === 128
      asUnsignedInteger(8)(-127.toByte) === 129
      asUnsignedInteger(8)(-126) === 130
      asUnsignedInteger(8)(-10) === 246
      asUnsignedInteger(8)(-1) === 255
      println(asUnsignedInteger(16)(345643234))
      asUnsignedInteger(4)(-1) === 15

    }
    "computes signed value" in {
      val signed = asSignedInteger(8)(_)
      val unsigned = asUnsignedInteger(8)(_)
      unsigned(signed(1)).toByte === 1
      unsigned(signed(127)).toByte === 127
      unsigned(signed(128)).toByte === -128
      unsigned(signed(255)).toByte === -1
    }
  }
}
