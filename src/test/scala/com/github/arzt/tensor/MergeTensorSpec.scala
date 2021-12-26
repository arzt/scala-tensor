package com.github.arzt.tensor

import com.github.arzt.tensor.MergeTensor._
import com.github.arzt.tensor.TensorImplicits._
import TensorImplicits.All
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class MergeTensorSpec extends AnyFreeSpec with Matchers {

  "Merge tensor should" - {
    "compute shape dim 1" in {
      val a = Tensor[Int](2, 2)
      val b = Tensor[Int](3, 2)
      val c = Array(
        a,
        b).asCol

      val shape = MergeTensor.getShape(c)
      shape === Seq(5, 2)
    }
    "compute shape dim 2" in {
      val a = Tensor[Int](2, 5)
      val b = Tensor[Int](2, 7)
      val c = Array(
        a, b).asRows(1)

      val shape = MergeTensor.getShape(c)
      shape === Seq(2, 12)
    }
    "compute shape dim 3" in {
      val a = Tensor[Int](1, 2, 5)
      val b = Tensor[Int](4, 2, 5)
      val c = Array(
        a, b).asTensor(2, 1, 1)

      val shape = MergeTensor.getShape(c)
      shape === Seq(5, 2, 5)
    }
    "comput shape 3d" in {
      val a = Tensor[Int](2, 2, 2)
      val b = Tensor[Int](2, 3, 2)
      val c = Tensor[Int](1, 2, 2)
      val d = Tensor[Int](1, 3, 2)
      val cat = Array(
        a, b, c, d).asTensor(2, 2, 1)
      val shape = MergeTensor.getShape(cat)
      shape === Seq(3, 5, 2)
    }
    "flatten nested tensor 1" in {
      val res =
        Array(
          1, 2, 3, 4,
          5, 6, 7, 8).asRows(2)
      val a = res(All, 0 until 2)
      val b = res(All, 2 until 4)
      val c = Array(a, b).asRows(1)
      val ints = MergeTensor.getShape(c)
      val parentMap =
        Array[Int](
          0, 0, 1, 1,
          0, 0, 1, 1)
      val childMap =
        Array[Int](
          0, 1, 0, 1,
          2, 3, 2, 3)
      val merged = new MergeTensor[Int](ints, c, parentMap, childMap)
      merged === res
    }
    "flatten tensor 2" in {
      val res =
        Array(
          1, 2, 3, 4,
          5, 6, 7, 8).asRows(2)
      val a = res(0, All)
      val b = res(1, All)
      val c = Array(a, b).asCols(1)
      val ints = MergeTensor.getShape(c)
      val parentMap =
        Array[Int](
          0, 0, 0, 0,
          1, 1, 1, 1)
      val childMap =
        Array[Int](
          0, 1, 2, 3,
          0, 1, 2, 3)
      val merged = new MergeTensor[Int](ints, c, parentMap, childMap)
      merged === res
    }
    "compute mapping 1" in {
      val res =
        Array(
          1, 2, 3, 4,
          5, 6, 7, 8).asRows(2)
      val a = res(All, 0 until 2)
      val b = res(All, 2 until 4)
      val c = Array(a, b).asRows(1)
      val (parent, child) = MergeTensor.getParentAndChildMap(c, getShape(c))
      parent.toSeq === Seq(0, 0, 1, 1, 0, 0, 1, 1)
      child.toSeq === Seq(0, 1, 0, 1, 2, 3, 2, 3)
    }
    "compute mapping 2" in {
      val res =
        Array(
          1, 2, 3, 4,
          5, 6, 7, 8).asRows(2)
      val a = res(0, All)
      val b = res(1, All)
      val c = Array(a, b).asCols(1)
      val (parent, child) = MergeTensor.getParentAndChildMap(c, getShape(c))
      child.toSeq === Seq(0, 1, 2, 3, 0, 1, 2, 3)
      parent.toSeq === Seq(0, 0, 0, 0, 1, 1, 1, 1)
    }
    "compute mapping 3" in {
      val res =
        Array(
          1, 2, 3, 4,
          5, 6, 7, 8).asRows(2)
      val a = res(All, 0 until 3)
      val b = res(All, 3)
      val c = Array(a, b).asRows(1)
      val (parent, child) = MergeTensor.getParentAndChildMap(c, getShape(c))
      child.toSeq === Seq(0, 1, 2, 0, 3, 4, 5, 1)
      parent.toSeq === Seq(0, 0, 0, 1, 0, 0, 0, 1)
    }
    "concat 3d" in {
      val expected = Array(
        1, 2,
        3, 4,

        5, 6,
        7, 8).asTensor(2, 2, 2)
      val a0 = expected(0: Index, 0, 0)
      val a1 = expected(0: Index, 0, 1)
      val a2 = expected(0: Index, 1, 0)
      val a3 = expected(0: Index, 1, 1)
      val a4 = expected(1: Index, 0, 0)
      val a5 = expected(1: Index, 0, 1)
      val a6 = expected(1: Index, 1, 0)
      val a7 = expected(1: Index, 1, 1)
      val t = Array(a0, a1, a2, a3, a4, a5, a6, a7).asTensor(2, 2, 2)
      val concat = t.concat()
      concat === expected
    }
    "horizontal non matching" in {
      val e = EchoTensor(Vector(2, 3))
      val r = Array(
        e(0: Index, 0), e(0, 1 :: 2),
        e(1, 0 :: 1), e(1: Index, 2))
        .asTensor(2, 2)
        .concat()
      r === e
    }
    "vertical matching" in {
      val e = EchoTensor(Vector(3, 2))
      val r = Array(
        e(0: Index, 0), e(0: Index, 1),
        e(1 :: 2, 0), e(1 :: 2, 1))
        .asTensor(2, 2)
        .concat()
      r === e
    }
    "vertical non matching shape" in {
      val e = EchoTensor(Vector(4, 2))
      val r = Array(
        e(0: Index, 0), e(0 :: 2, 1),
        e(1 :: 3, 0), e(3: Index, 1))
        .asTensor(2, 2)
      val res = MergeTensor.getShape(r)
      res === Seq(4, 2)

      val (parent, child) = MergeTensor.getParentAndChildMap(r, res)
      parent(0) === 0
      parent(1) === 1
      parent(2) === 2
      parent(3) === 1
      parent(4) === 2
      parent(5) === 1
      parent(6) === 2
      parent(7) === 3
      child(0) === 0
      child(1) === 0
      child(2) === 0
      child(3) === 1
      child(4) === 1
      child(5) === 2
      child(6) === 2
      child(7) === 0
    }
    "vertical non matching" in {
      val e = EchoTensor(Vector(3, 2))
      val value = Array(
        e(0: Index, 0), e(0 :: 1, 1),
        e(1 :: 2, 0), e(2: Index, 1))
        .asTensor(2, 2)
      val r = value
        .concat()
      r === e
    }
    "simple 3D test" in {
      val e = EchoTensor(Vector(2, 1, 3))
      val t = Array(
        e(0: Index, 0, 0), e(0: Index, 0, 1 :: 2),
        e(1, 0, 0 :: 1), e(1: Index, 0, 2))
        .asTensor(2, 1, 2)
      val r = t
        .concat()
      e === r
    }
    "simple 3D test2" in {
      val e = EchoTensor(Vector(2, 3, 2))
      val t = Array(
        e(0: Index, 0, 0), e(0: Index, 0 :: 1, 1),
        e(0: Index, 1 :: 2, 0), e(0: Index, 2, 1),

        e(1, 0 :: 1, 0), e(1: Index, 0, 1),
        e(1: Index, 2, 0), e(1: Index, 1 :: 2, 1))
        .asTensor(2, 2, 2)
      val r = t.concat()
      e === r
    }

    "simpler simpler advanced 3D test" in {
      val e = EchoTensor(Vector(3, 1, 2))
      val t = Array(
        e(0 :: 1, 0 :: 0, 0 :: 0),
        e(0 :: 0, 0 :: 0, 1 :: 1),

        e(2 :: 2, 0 :: 0, 0 :: 0),
        e(1 :: 2, 0 :: 0, 1 :: 1))
        .asTensor(2, 1, 2)
      val shape = MergeTensor.getShape(t)
      val r = t.concat()
      r === e
    }
    "simpler simpler advanced 2D test" in {
      val e = EchoTensor(Vector(1, 3, 2))
      val t = Array(
        e(0 :: 0, 0 :: 1, 0 :: 0),
        e(0 :: 0, 0 :: 0, 1 :: 1),

        e(0 :: 0, 2 :: 2, 0 :: 0),
        e(0 :: 0, 1 :: 2, 1 :: 1))
        .asTensor(1, 2, 2)
      val shape = MergeTensor.getShape(t)
      val r = t.concat()
      r === e
    }
    "unmatching shape size" in {
      val res = Array(EchoTensor(Vector(3)), EchoTensor(Vector(3))).asCol.concat()
      res.shape(0) === 2
      res.shape(1) === 3
    }
    "simple test" in {
      val in = Array(2.constTensor(2, 1), 2.constTensor(2, 1)).asRow
      val a = in.concat()
      a === 2.constTensor(2, 2)
    }
  }

}
