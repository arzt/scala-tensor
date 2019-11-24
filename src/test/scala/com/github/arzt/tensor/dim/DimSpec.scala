package com.github.arzt.tensor.dim

import org.specs2.mutable.Specification

import scala.collection.immutable.ArraySeq

class DimSpec extends Specification {
  "Dimension" should {
    "Single" in {
      val d = Dim(3)
      d.shape === List(3)
      d.length === 3
      d === ArraySeq(0, 1, 2)
    }
    "Single and index seq" in {
      val d = Dim(4, Seq(0, 0, 3))
      d.shape === List(3)
      d.length === 3
      d === ArraySeq(0, 0, 3)
    }
    "Single and index seq with negative elements" in {
      val d = Dim(4, Seq(0, -1, 3, 5, -4))
      d.shape === List(5)
      d.length == 3
      d === ArraySeq(0, 3, 3, 1, 0)
    }
    "Two dimensions" in {
      val d = Dim(2, Dim(2))
      val dimList = List(2, 2)
      d.shape === dimList
      d.length === dimList.product
      d === ArraySeq(0, 1, 2, 3)
    }
    "Recursive" in {
      val d = Dim(3, Dim(2))
      val dimList = List(3, 2)
      d.shape === dimList
      d.length === dimList.product
      d === ArraySeq(0, 1, 2, 3, 4, 5)
    }
    "Recursive and sub seq 1" in {
      val d = Dim(3, Seq(0, 0, 0), Dim(2))
      val dimList = List(3, 2)
      d.shape === dimList
      d.length === dimList.product
      d === ArraySeq(0, 1, 0, 1, 0, 1)
    }
    "Recursive and sub seq 2" in {
      val d = Dim(3, Dim(1, Seq(0, 0)))
      val dimList = List(3, 2)
      d.shape === dimList
      d.length === dimList.product
      d === ArraySeq(0, 0, 2, 2, 4, 4)
    }
    "test mod" in {
      val d = Dim(3, Dim(1, Seq(0, 0)))
      val result = d(Seq(0), _(Seq(3)))
      result === ArraySeq(0)
    }
    "from list" in {
      val dims = List(3, 4, 5)
      val d = Dim(dims)
      d.length === dims.product
      d.shape === dims
    }
    "empty dim" in {
      val empty = Dim(List(3, 0, 4))
      empty === Seq()
      empty.length === 0
      empty.isEmpty === true
    }
  }
}
