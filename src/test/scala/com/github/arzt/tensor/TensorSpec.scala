package com.github.arzt.tensor

import com.github.arzt.tensor.TensorImplicits._
import org.specs2.mutable.Specification

class TensorSpec extends Specification {
  "Tensors" should {
    "read elements by index" in {
      val data = Array[Int](0, 1, 2, 3, 4, 5, 6, 7)
      val t1 = Tensor(Vector(2, 4), data)
      val t2 = Tensor(Vector(2, 2, 2), data)
      t1(0) === 0
      t1(4) === 4
      t1(7) === 7
      t1(0, 0) === 0
      t1(1, 1) === 5
      t1(1, 3) === 7
      t1(1, 1) === 5
      t2(0, 0, 0) === 0
      t2(0, 0, 1) === 1
      t2(0, 1, 0) === 2
      t2(0, 1, 1) === 3
      t2(1, 1, 1) === 7
    }
    "update elements by index" in {
      val data = Array[Double](
        1, 2, 3, 4,
        5, 6, 7, 8,
        9, 10, 11, 12)
      val t = Tensor(Vector(1, 12), data)
      t(0) = 7
      t(0) === 7
      val t1 = Tensor(Vector(3, 4), data)
      t1(2, 3) = 8
      t1(2, 3) === 8
      val t2 = Tensor(Vector(2, 3, 2), data)
      t2(0, 1, 1) = -5
      t2(0, 1, 1) === -5
    }
    "read indexed data" in {
      val t = Tensor(Vector(3, 3), Array(1, 2, 3, 4, 5, 6, 7, 8, 9))
      val r = t(1 until 2, 1 until 2)
      r(0) === 5
    }
    "have string representation" in skipped {
      val strings = Array[String]("a", "th\nis", "is", "my\n   tes\n t").asRows(2)
      val rep = strings.toString

      val data = Array[Int](1, 2, 3, 4)
      Tensor(Vector(2, 2), data).toString mustEqual " 1 2\n 3 4\n"
      Tensor(Vector(4, 1), data).toString mustEqual " 1\n 2\n 3\n 4\n"
      Tensor(Vector(1, 4), data).toString mustEqual " 1 2 3 4\n"
    }
    "update whole tensor" in {
      val data = Array[Int](
        1, 2, 0, 0,
        0, 0, 0, 0)
      val zeros = new Array[Int](8)
      val a = Tensor(Vector(2, 4), data)
      val b = Tensor(Vector(2, 4), zeros)
      b := a
      data.toSeq === zeros.toSeq
    }
    "copy sub-tensors" in {
      val data = Array[Int](
        1, 2, 0, 0,
        0, 0, 0, 0)
      val t = Tensor(Vector(2, 4), data)
      t(1 to 1, 2 to 3) = t(0 to 0, 0 to 1)
      val expected = Seq(
        1, 2, 0, 0,
        0, 0, 1, 2)
      data.toSeq === expected
    }
    "force" in {
      val data = Array[Int](
        1, 2, 0, 0,
        0, 0, 0, 0)
      val t = Tensor(Vector(2, 4), data)
      val t2 = t()
      t2.toSeq === t.toSeq
    }
    "linear view" in {
      import TensorImplicits._
      val view = Array[Int](
        1, 2, 3,
        4, 5, 6)
        .asTensor(2, 3)
        .apply(0 to 1, 1 to 1)
        .toSeq
      view.view.force === Seq(2, 5)
    }
    "equal" in {
      val a = Tensor(Vector(3), Array(1, 2, 3))
      val b = Tensor(Vector(3), Array(1, 2, 3))
      val c = Tensor(Vector(3), Array(1, 2, 4))
      val d = Tensor(Vector(1, 3), Array(1, 2, 3))
      val e = Tensor(Vector(3), Array(0, 0, 1, 2, 3), 2)
      a === a
      b === a
      a === b
      a !== c
      b !== c
      a === d
      a === e
    }
    "fail on wrong shape " in {
      new ArrayTensor(Vector(-1), Array[Int]()) must throwA[IllegalArgumentException]
    }
    "create vector" in {
      val t = Tensor(Array(1, 2, 3))
      t.toSeq === Seq(1, 2, 3)
    }
    "get nrows and ncols" in {
      val t = Tensor(Vector(3, 4))
      t.cols === 4
      t.rows === 3
    }
    "get isMatrix isVector" in {
      val vec = Tensor(4)
      vec.isMatrix === false
      vec.isVector === true
      val mat = Tensor(4, 4)
      mat.isVector === false
      mat.isMatrix === true
    }
    "support stepped indexing" in {
      val t = Tensor(Array(0, 1, 2, 3, 4, 5, 6, 7, 8))
      t(1 until 8 by 3).toSeq === Seq(1, 4, 7)
    }
    "support addition" in {
      val t = Tensor(Array(1, 2, 3, 4, 5, 6, 7, 8, 9))
      val h = t + 4
      h.toSeq === Array(1, 2, 3, 4, 5, 6, 7, 8, 9).map(_ + 4).toSeq
    }
    "support tensor addion" in {
      val a = Tensor(Array(1, 2, 3, 4))
      val b = Tensor(Array(1, 2, 3, 4))
      val c = a + b
      c.toSeq.toArray.toSeq === Array(2, 4, 6, 8).toSeq
    }
    "reassignment" in {
      val res = Array(0, 0, 0, 0)
      val a = Array(1, 2, 3, 4)
      val b = Array(4, 3, 2, 1)
      val sh = Vector(4)
      val tres = new ArrayTensor(sh, res)
      val ta = new ArrayTensor(sh, a)
      val tb = new ArrayTensor(sh, b)
      tres := ta + tb
      res.toSeq === Array(5, 5, 5, 5).toSeq
    }
    "pointwise ==" in {
      val t = Array(1, 2, 1, 2, 1, 1, 2).asRow
      val exp = Array(false, true, false, true, false, false, true).asRow
      (t == 2) === exp
    }
    "int seq indexing" in {
      val t = Array(1, 2, 3, 4).asRow
      t(Seq(0, 3, 0, 0, 1)) === Array(1, 4, 1, 1, 2).asRow
    }
    "bool seq indexing" in {
      val t = Array(1, 2, 3, 4).asTensor(4)
      t(Seq(true, false, false, true)) === Array(1, 4).asTensor(2)
    }
    "integer tensor indexing" in {
      val t = Array(1, 2, 3, 4).asTensor(4)
      val idx = Array(3, 2, 1, 0).asTensor(4)
      t(idx) === Array(4, 3, 2, 1).asTensor(4)
    }
    "boolean tensor indexing" in {
      val t = Array(1, 2, 3, 4).asTensor(4)
      val idx = Array[Boolean](true, false, false, true).asTensor(4)
      t(idx) === Array(1, 4).asTensor(2)
    }
    "boolean tensor indexing 2" in {
      val label = Array(0, 1, 0, 2, 3, 3, 2).asRow
      val t =
        Array(0, 1, 0, 1,
          4, 5, 6, 7)
          .asTensor(2, 4)
      val idx = t(0, ::) == 1
      val r = t(1, idx).equals(Array(5, 7).asTensor(2))
      r === true
    }
    "advanced boolean tensor indexing" in {
      val t = Array(1, 2, 3, 4, 5, 6, 7, 8).asTensor(8)
      t(t.map(_ % 2 == 0)) === Array(2, 4, 6, 8).asTensor(4)
    }
    "range indexing" in {
      val t = Array(1, 2, 3, 4).asTensor(4)
      t(0 :: 3) === t
    }
    "stepped indexing" in {
      val t = Array(1, 2, 3, 4, 5).asTensor(5)
      t(0 :: 4 :: 2) === Array(1, 3, 5).asTensor(3)
    }
    "negative steps" in {
      val t = Array(1, 2, 3, 4).asTensor(4)
      t(3 :: 0 :: -1) === Array(4, 3, 2, 1).asTensor(4)
    }
    "negative indices" in {
      val t = Array(1, 2, 3, 4).asRow
      t(-2 :: -1) === Array(3, 4).asRow
    }
    "reverse" in {
      val t = Array(1, 2, 3, 4).asRow
      t(-::) === Array(4, 3, 2, 1).asRow
    }
    "reverse 2D" in {
      val t =
        Array(1, 2,
          3, 4)
          .asRows(2)
      t(::, -::) ===
        Array(2, 1,
          4, 3)
        .asRows(2)
    }
    "revers 3D" in {
      val result =
        Array(
          1,
          2,
          3,
          4,
          5,
          6)
          .asTensor(3, 2, 1)
          .apply(-::, ::, ::)
      val expected =
        Array(
          5,
          6,
          3,
          4,
          1,
          2)
          .asTensor(3, 2, 1)
      result === expected
    }
    "negative indices negative step size" in {
      val t = Array(1, 2, 3, 4, 5, 6).asRow
      val tuple: Index = -1 :: -5 :: -2
      val seq = tuple(6).toArray.toSeq
      t(tuple) === Array(6, 4, 2).asRow
    }
    "advance indexing" in {
      val t =
        Array(1, 2, 3, 4,
          5, 6, 7, 8,
          9, 10, 11, 12,
          13, 14, 15, 16)
          .asTensor(4, 4)
      val result = t(0, 2 :: -1)
      result === Array(3, 4).asTensor(1, 2)
    }
    "test" in {
      val t = Array(1, 2, 3, 4).asRow
      val b = t(::)
      t === b
    }
    "math" in {
      val t1 = Array[Double](4, 5).asRow
      t1 + 4 === Array[Double](8, 9).asRow
      t1 - 4 === Array[Double](0, 1).asRow
      t1 * 2 === Array[Double](8, 10).asRow
      val a = Array[Int](1, 2).asRow
      a.map(_ / 2) === Array[Int](0, 1).asRow
    }
    "convert" in {
      val t1 = Array[Int](4, 5).asRow
      t1.toDouble === Array[Double](4, 5).asRow
    }
  }
}
