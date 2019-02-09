package com.github.arzt.tensor

import org.specs2.mutable.Specification
import TensorImplicits._
import MergeTensor._

class MergeTensorSpec extends Specification {

  "Merge tensor" should {
    "compute shape dim 1" in {
      val a = Tensor[Int](2,2)
      val b = Tensor[Int](3,2)
      val c = Array(
        a,
        b
      ).asCol

      val shape = MergeTensor.getShape(c)
      shape === Seq(5, 2)
    }
    "compute shape dim 2" in {
      val a = Tensor[Int](2,5)
      val b = Tensor[Int](2,7)
      val c = Array(
        a, b
      ).asRows(1)

      val shape = MergeTensor.getShape(c)
      shape === Seq(2, 12)
    }
    "compute shape dim 3" in {
      val a = Tensor[Int](1, 2,5)
      val b = Tensor[Int](4, 2,5)
      val c = Array(
        a, b
      ).asTensor(2, 1, 1)

      val shape = MergeTensor.getShape(c)
      shape === Seq(5, 2, 5)
    }
    "comput shape 3d" in {
      val a = Tensor[Int](2, 2, 2)
      val b = Tensor[Int](2, 3, 2)
      val c = Tensor[Int](1, 2, 2)
      val d = Tensor[Int](1, 3, 2)
      val cat = Array(
        a, b, c, d
      ).asTensor(2, 2, 1)
      val shape = MergeTensor.getShape(cat)
      shape === Seq(3, 5, 2)
    }
    "flatten nested tensor 1" in {
      val res =
        Array(
          1, 2, 3, 4,
          5, 6, 7, 8
        ).asRows(2)
      val a = res(::, 0 until 2)
      val b = res(::, 2 until 4)
      val c = Array(a, b).asRows(1)
      val ints = MergeTensor.getShape(c)
      val parentMap =
        Array[Int](
          0, 0, 1, 1,
          0, 0, 1, 1
        )
      val childMap =
        Array[Int](
          0, 1, 0, 1,
          2, 3, 2, 3
        )
      val merged = new MergeTensor[Int](ints, c, parentMap, childMap)
      merged === res
    }
    "flatten nested tensor 2" in {
      val res =
        Array(
          1, 2, 3, 4,
          5, 6, 7, 8
        ).asRows(2)
      val a = res(0, ::)
      val b = res(1, ::)
      val c = Array(a, b).asCols(1)
      val ints = MergeTensor.getShape(c)
      val parentMap =
        Array[Int](
          0, 0, 0, 0,
          1, 1, 1, 1
        )
      val childMap =
        Array[Int](
          0, 1, 2, 3,
          0, 1, 2, 3
        )
      val merged = new MergeTensor[Int](ints, c, parentMap, childMap)
      merged === res
    }
    "compute parent mapping 1" in {
      val res =
        Array(
          1, 2, 3, 4,
          5, 6, 7, 8
        ).asRows(2)
      val a = res(::, 0 until 2)
      val b = res(::, 2 until 4)
      val c = Array(a, b).asRows(1)
      val (parent, child) = MergeTensor.getParentAndChild(c, getShape(c))
      parent.toSeq === Seq(0, 0, 1, 1, 0, 0, 1, 1)
    }
    "compute parent mapping 2" in {
      val res =
        Array(
          1, 2, 3, 4,
          5, 6, 7, 8
        ).asRows(2)
      val a = res(0, ::)
      val b = res(1, ::)
      val c = Array(a, b).asCols(1)
      val (parent, child) = MergeTensor.getParentAndChild(c, getShape(c))
      parent.toSeq === Seq(0, 0, 0, 0, 1, 1, 1, 1)
    }
    "compute parent mapping 3" in {
      val res =
        Array(
          1, 2, 3, 4,
          5, 6, 7, 8
        ).asRows(2)
      val a = res(0, ::)
      val b = res(1, ::)
      val c = Array(a, b).asCols(1)
      val (parent, child) = MergeTensor.getParentAndChild(c, getShape(c))
      parent.toSeq === Seq(0, 0, 0, 0, 1, 1, 1, 1)
    }
  }

}
