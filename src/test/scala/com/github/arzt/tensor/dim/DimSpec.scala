package com.github.arzt.tensor.dim

import org.specs2.mutable.Specification

class DimSpec extends Specification {
  "Dimension" should {
    "work" in {
      1 === 1
    }
    "Unit" in {
      val a = Dim(11)
      val b = Dim(4, a)
      val d = Dim(2, b)
      val c = b.dims
      d.length
      c === List(4, 11)
      d.dims === List(2, 4, 11)
      b.length === 44
      println(a)
      println(b)

      1 === 1
    }
  }
}
