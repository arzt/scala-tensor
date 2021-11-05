package com.github.arzt.tensor.dim

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.compat.immutable.ArraySeq
import scala.collection.immutable.Seq

class DimSpec extends AnyFreeSpec with Matchers {
  "Dimension should" - {
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
      val d = Dim(3, Dim(2, Seq(0, 0)))
      val dimList = List(3, 2)
      d.shape === dimList
      d.length === dimList.product
      d === ArraySeq(0, 0, 2, 2, 4, 4)
    }
    "from list" in {
      val dims = List(3, 4, 5)
      val d = Dim.fromDimensions(dims)
      d.length === dims.product
      d.shape === dims
    }
    "empty dim" in {
      val empty = Dim.fromDimensions(List(3, 0, 4))
      empty === Seq()
      empty.length === 0
      empty.isEmpty === true
    }
    "sub test" in {
      Dim(4, Seq(0), Dim(4, Seq(3))) === List(3)
      Dim(4, Seq(1), Dim(4, Seq(0))) === List(4)
      Dim(4, Seq(1), Dim(4, Seq(3))) === List(7)
      Dim(4, Seq(2), Dim(4, Seq(0))) === List(8)
      Dim(4, Seq(2), Dim(4, Seq(3))) === List(11)
      Dim(4, Seq(3), Dim(4, Seq(3))) === List(15)
    }
  }
}
