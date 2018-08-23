package com.github.arzt.tensor

import com.github.arzt.tensor.TensorImplicits._
import org.specs2.mutable.Specification

import scala.util.Random

class TensorSpec extends Specification {
  "Tensors" should {
    "read elements by index" in {
      val data = Array[Int](0, 1, 2, 3, 4, 5, 6, 7)
      val t1 = Tensor(Vector(2, 4), data)
      val t2 = Tensor(Vector(2, 2, 2), data)
      t1.isView === false
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
    "tensor with offset" in {
      val t = Tensor(Vector(1, 1), Array(1, 2, 3), 1)
      t(0, 0) === 2
      t.apply(0, 1) === 3
    }
    "tensor with data" in {
      val t = Tensor(Array(1))
      t(0, 0) === 1
    }
    "tesor with data and offset" in {
      val t = Tensor(Array(1, 2), 1)
      t(0, 0) === 2
    }
    "read 4d tensor" in {
      val t = Array[Int](
        1, 2,
        3, 4,
        5, 6,
        7, 8)
        .asTensor(2, 2, 1, 2)
      val b = t(::, ::, ::, ::).apply()
      t(0, 0, 0, 0) === 1
      t(0, 1, 0, 1) === 4
      t(1, 1, 0, 1) === 8
      b === t
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
    "equality" in {
      Array(1).asRow === Array(1).asRow
      Array(1).asRow !== Array(2).asRow
      Array(1).asRow !== 4
    }
    "as matrix by number of cols" in {
      val a = Array(1, 2, 3).asCols(3)
      val b = Tensor(Vector(1, 3), Array(1, 2, 3))
      a === b
    }
    "as column" in {
      val a = Array(1, 2, 3).asCol
      val b = Tensor(Vector(3, 1), Array(1, 2, 3))
      a === b
    }
    "with offset" in {
      val a = Array(1, 2, 3).withOffset(1).asTensor(2)
      val b = Array(2, 3).asRow
      a === b
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
    "update with tensor 1D" in {
      val a = Array(1, 0, 0, 4).asRow
      val b = Array(2, 3).asRow
      a(1 :: 2) = b
      a === Array(1, 2, 3, 4).asRow
    }
    "update with tensor 3D" in {
      val a = Array(1, 0,
        0, 4,
        5, 6).asTensor(3, 1, 2)
      a(1, ::, ::) = Array(4, 6).asTensor(1, 1, 2)
      a === Array(1, 0, 4, 6, 5, 6).asTensor(3, 1, 2)
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
      t(::).isView === true
    }
    "mapping" in {
      val t = Array(1).asRow.map(_.toString)
      t(0, 0) === "1"
      t.isView === true
      (t(0, 0) = "test") must throwA[UnsupportedOperationException]
    }
    "support addition" in {
      val t = Tensor(Array(1, 2, 3, 4, 5, 6, 7, 8, 9))
      val h = t + 4
      h.toSeq === Array(1, 2, 3, 4, 5, 6, 7, 8, 9).map(_ + 4).toSeq
    }
    "reassignment" in {
      val res = Array(0, 0, 0, 0)
      val ta = res.asRow
      val tensor = Array(5, 5, 5, 5).asRow
      ta() = tensor
      res.toSeq === Array(5, 5, 5, 5).toSeq
    }
    "pointwise ==" in {
      val t = Array(1, 2, 1, 2, 1, 1, 2).asRow
      val exp = Array(false, true, false, true, false, false, true).asRow
      val exp2 = !exp
      (t == 2) === exp
      (t != 2) === exp2
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
    "numeric operations" in {
      val t1 = Array[Double](4, 5).asRow
      t1 + 4 === Array[Double](8, 9).asRow
      t1 - 4 === Array[Double](0, 1).asRow
      t1 * 2 === Array[Double](8, 10).asRow
      val a = Array[Int](1, 2).asRow
      a.map(_ / 2) === Array[Int](0, 1).asRow
      val a2 = Array(1, 2, 3, 4).asRow
      val b2 = Array(1, 2, 3, 4).asRow
      (a2 + b2).isView === true
      ((a2 + b2)(0) = 5) must throwAn[UnsupportedOperationException]
      (a2 + b2) === Array(2, 4, 6, 8).asRow
      (a2 - b2) === Array(0, 0, 0, 0).asRow
      (a2 * b2) === Array(1, 4, 9, 16).asRow
    }
    "convert" in {
      val t1 = Array[Int](4, 5).asRow
      t1.toDouble === Array[Double](4, 5).asRow
    }
    "boolean tensor operators" in {
      val a = Array(true, false).asRow
      val b = Array(true, false, true).asRow
      val c = Array(true, false, false).asRow
      !a === Array(false, true).asRow
      (a && true) === Array(true, false).asRow
      (a || true) === Array(true, true).asRow
      (a ^ true) === Array(false, true).asRow
      (b && c) === Array(true, false, false).asRow
      (b || c) === Array(true, false, true).asRow
      (b ^ c) === Array(false, false, true).asRow
    }
    "permute dimensions" in {
      val t = Array(
        0, 1, 2,
        3, 4, 5)
        .asTensor(2, 3)
      val result0 = t.permute(Seq(0, 1))
      result0.shape === Seq(2, 3)
      result0 === t

      val result1 = t.permute(Seq(1, 0))
      val expected =
        Array(
          0, 3,
          1, 4,
          2, 5)
          .asTensor(3, 2)
      result1.shape === Seq(3, 2)
      result1 === expected
    }
    "permute more than 2 dimensions" in {
      val tensor =
        Array(0, 1,

          2, 3,

          4, 5,

          6, 7,

          8, 9,

          10, 11)
          .asTensor(2, 3, 1, 2)
      val intermediate = tensor.permute(Array(1, 2, 3, 0))
      val result = intermediate.permute(Array(3, 0, 1, 2))
      intermediate !== tensor
      result === tensor
    }
    "transpose" in {
      val tensor =
        Array(0, 1,
          2, 3,
          4, 5,

          0, 1,
          2, 3,
          4, 5)
          .asTensor(2, 3, 2)
      val expected =
        Array(0, 2, 4,
          1, 3, 5,

          0, 2, 4,
          1, 3, 5)
          .asTensor(2, 2, 3)
      tensor.t === expected
      tensor.t.t === tensor
    }
    "mmul" in {
      val a =
        Array[Double](3, 2, 1)
          .asRows(1)
      val b =
        Array[Double](
          1,
          2,
          9)
          .asRows(3)
      val c = a ** b
      val d = b ** a
      val expected =
        Array[Double](16)
          .asRows(1)
      val expectod2 =
        Array[Double](3, 2, 1,
          6, 4, 2,
          27, 18, 9)
          .asRows(3)
      c === expected
      d.shape === collection.immutable.Seq(3, 3)
    }
    "mmul2" in {
      val a =
        Array[Double](1, 2,
          3, 4,
          5, 6)
          .asRows(3)
      val b =
        Array[Double](1, 2, 3,
          4, 5, 6)
          .asRows(2)
      val c = a ** b
      val d = b ** a
      val expetced2 =
        Array[Double](22, 28,
          49, 64)
          .asRows(2)
      val expected =
        Array[Double](9, 12, 15,
          19, 26, 33,
          29, 40, 51)
          .asRows(3)
      c === expected
      d === expetced2
    }
    "mmul big matrix" in {
      val M = 300
      val K = 300
      val N = 300
      val r = new Random()
      val A = Array.fill(M * K)(r.nextDouble()).asRows(M)
      val B = Array.fill(K * N)(r.nextDouble()).asRows(K)
      val C = A ** B
      C.shape === Vector(M, N)
    }
    "mmul multi-dimensional" in {
      val A =
        Array[Double](1, 2, 3,
          4, 5, 6,
          7, 8, 9)
          .asTensor(3, 1, 3)
      val B =
        Array[Double](3, 2, 1,
          6, 5, 4,
          9, 8, 7)
          .asTensor(3, 3, 1)
      val C = A ** B
      val expected =
        Array[Double](10, 73, 190)
          .asTensor(3, 1, 1)
      C.shape === Vector(3, 1, 1)
      C === expected
    }
    "mmul transposed" in {
      val a =
        Array[Double](1, 2,
          4, 5)
          .asRows(2)
      val b =
        Array[Double](1, 0,
          0, 1)
          .asRows(2)
      val c = a.t ** b
      val expected =
        Array[Double](1, 4,
          2, 5)
          .asRows(2)
      expected === c
    }
    "single precision mmul" in {
      val a =
        Array[Float](1, 2,
          4, 5)
          .asRows(2)
      val b =
        Array[Float](1, 0,
          0, 1)
          .asRows(2)
      val c = a.t ** b
      val expected =
        Array[Float](1, 4,
          2, 5)
          .asRows(2)
      expected === c
    }
    "reshape" in {
      val data = Array(1, 2)
      val x = data.asRow
      val in = x.reshape(Array(2, 1))
      in === Array(1, 2).asCol
      x.reshape(Seq(5)) must throwA[IllegalArgumentException]
      x.asCol() === Array(1, 2).asCol
      x.asCol().asRow === Array(1, 2).asRow
      in.isView === true
      in(0) = 5
      in(0) === 5
    }
    "drop singular dimension" in {
      val t = Array[Int](
        1, 2, 3)
        .asTensor(1, 3, 1)
      t.dropSingular(2).shape === Seq(1, 3)
      t.dropSingular(1) must throwA[IllegalArgumentException]
    }
    "add singular dimension" in {
      val t = Array(1, 2, 3, 4).asCols(2).addSingular(1)
      t.shape === Seq(2, 1, 2)
    }
    "Indexing tensor" in {
      val shape = Vector(2, 3, 4)
      val t = IndexTensor(shape)
      (0 until t.length)
        .foreach { i =>
          println(t(i))
        }
      t.length === shape.product
    }
    "dissect tensor" in {
      val t = Array(
        0,1,2,
        3,4,5
      )
        .asTensor(3, 2, 1)
        .dissect(1, 2)
      val expectet =
        Array[Tensor[Int]](

        )
        .asTensor(3,1,1)
      t === expectet
    }
  }
}
