package com.github.arzt.tensor

import com.github.arzt.tensor.TensorImplicits._
import com.github.arzt.tensor.convert.LongToIntConverter
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.Seq
import scala.util.Random

class TensorSpec extends AnyFreeSpec with Matchers {
  "Tensors should" - {
    "read elements by index" in {
      val data = Array[Int](0, 1, 2, 3, 4, 5, 6, 7)
      val t1 = Tensor(data, 2, 4)
      val t2 = Tensor(data, 2, 2, 2)
      t1.isView === false
      t1(0) === 0
      t1(4) === 4
      t1(7) === 7
      t1(0, 0)(0) === 0
      t1(1, 1)(0) === 5
      t1(1, 3)(0) === 7
      t1(1, 1)(0) === 5
      t2(0, 0, 0)(0) === 0
      t2(0, 0, 1)(0) === 1
      t2(0, 1, 0)(0) === 2
      t2(0, 1, 1)(0) === 3
      t2(1, 1, 1)(0) === 7
    }
    "tensor with offset" in {
      val t = Tensor(1, Array(1, 2, 3), 1, 1)
      t(0, 0)(0) === 2
      t(0, 1)(0) === 2
    }
    "tensor with data" in {
      val t = Tensor(Array(1))
      t(0) === 1
    }
    "tesor with data and offset" in {
      val t = Tensor(1, Array(1, 2))
      t(0) === 2
    }
    "read 4d tensor" in {
      val t = Array[Int](
        1, 2,
        3, 4,
        5, 6,
        7, 8)
        .asTensor(2, 2, 1, 2)
      val b = t(All, All, All, All)
      t(0, 0, 0, 0)(0) === 1
      t(0, 1, 0, 1)(0) === 4
      t(1, 1, 0, 1)(0) === 8
      b === t
    }
    "update elements by index" in {
      val data = Array[Double](
        1, 2, 3, 4,
        5, 6, 7, 8,
        9, 10, 11, 12)
      val t = Tensor(data, 1, 12)
      t(0) = 7
      t(0) === 7
      val t1 = Tensor(data, 3, 4)
      t1(2, 3)(0) = 8
      t1(2, 3)(0) === 8
      val t2 = Tensor(data, 2, 3, 2)
      t2(0, 1, 1) = Tensor(Array(-5.0))
      t2(0, 1, 1)(0) === -5.0
    }
    "read indexed data" in {
      val t = Tensor(Array(1, 2, 3, 4, 5, 6, 7, 8, 9), 3, 3)
      val r = t(1 until 2, 1 until 2)
      r(0) === 5
    }
    "equality" in {
      Array(1).asRow === Array(1).asRow
      Array(1).asRow !== Array(2).asRow
      Array(1).asRow !== 4
      Array(1, 2, 3).asRow === Array(1, 2, 3).asCol
      Array(1, 2, 3, 4).asRows(1) !== Array(1, 2, 3, 4).asRows(2)
    }
    "as matrix by number of cols" in {
      val a = Array(1, 2, 3).asCols(3)
      val b = Tensor(Array(1, 2, 3), 1, 3)
      a === b
    }
    "as column" in {
      val a = Array(1, 2, 3).asCol
      val b = Tensor(Array(1, 2, 3), 3, 1)
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
      val a = Tensor(data, 2, 4)
      val b = Tensor(zeros, 2, 4)
      b !== a
      b := a
      b === a
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
      a(1, All, All) = Array(4, 6).asTensor(1, 1, 2)
      a === Array(1, 0, 4, 6, 5, 6).asTensor(3, 1, 2)
    }
    "copy sub-tensors" in {
      val data = Array[Int](
        1, 2, 0, 0,
        0, 0, 0, 0)
      val t = Tensor(data, 2, 4)
      t(1 to 1, 2 to 3) = t(0 to 0, 0 to 1)
      val expected = Seq(
        1, 2, 0, 0,
        0, 0, 1, 2)
      t.sameElements(expected)
    }
    "cache" in {
      val t = EchoTensor(Seq(2, 4))
      val t2 = t.cached
      t2 === t
    }
    "cache parallel" in {
      val data = Array[Int](
        1, 2, 0, 0,
        0, 0, 0, 0)
      val t = EchoTensor(Seq(2, 4, 500))
      val t2 = t.cachedParallel
      t2 === t
    }
    "linear view" in {
      import TensorImplicits._
      val tensor = Array[Int](1, 2, 3, 4, 5, 6)
        .asTensor(2, 3)
        .apply(0 to 1, 1)

      tensor.sameElements(Seq(2, 5))
    }
    "equal" in {
      val a = Tensor(Array(1, 2, 3), 3)
      val b = Tensor(Array(1, 2, 3), 3)
      val c = Tensor(Array(1, 2, 4), 3)
      val d = Tensor(Array(1, 2, 3), 1, 3)
      val e = Tensor(2, Array(0, 0, 1, 2, 3), 3)
      a === a
      b === a
      a === b
      a !== c
      b !== c
      a === d
      a === e
    }
    "fail on wrong shape " in {
      an[IllegalArgumentException] should be thrownBy new ArrayTensor(Vector(-1), Array[Int]())
    }
    "create vector" in {
      val t = Tensor(Array(1, 2, 3))
      t.sameElements(Seq(1, 2, 3))
    }
    "read nrows and ncols" in {
      val t = Tensor[Int](3, 4)
      t.cols === 4
      t.rows === 3
    }
    "read isMatrix isVector" in {
      val vec = Tensor[Any](4)
      vec.isMatrix === false
      vec.isVector === true
      val mat = Tensor[Any](4, 4)
      mat.isVector === false
      mat.isMatrix === true
    }
    "support stepped indexing" in {
      val t = Tensor(Array(0, 1, 2, 3, 4, 5, 6, 7, 8))
      t(1 until 8 by 3).sameElements(Seq(1, 4, 7))
      t(All).isView === true
    }
    "mapping" in {
      val t = Array(1).asRow.map(_.toString)
      t(0) === "1"
      t.isView === true
      an[UnsupportedOperationException] should be thrownBy (t(0) = Tensor(Array("test")))
    }
    "support addition" in {
      val t = Tensor(Array(1, 2, 3, 4, 5, 6, 7, 8, 9))
      val h = t + 4
      h.sameElements(Array(1, 2, 3, 4, 5, 6, 7, 8, 9).map(_ + 4))
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
      val idx = Array(3, -2, 1, 0).asTensor(4)
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
      val idx = t(0, All) == 1
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
      t(Reverse) === Array(4, 3, 2, 1).asRow
    }
    "reverse 2D" in {
      val t =
        Array(1, 2,
          3, 4)
          .asRows(2)
      t(All, Reverse) ===
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
          .apply(Reverse, All, All)
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
      val b = t(All)
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
      an[UnsupportedOperationException] should be thrownBy ((a2 + b2)(0) = 5)
      (a2 + b2) === Array(2, 4, 6, 8).asRow
      (a2 - b2) === Array(0, 0, 0, 0).asRow
      (a2 * b2) === Array(1, 4, 9, 16).asRow
      -t1 === t1 * -1
      (t1 * (-1)).abs === Array[Double](4, 5).asRow
      (t1 * 2) / 2 === t1
      (t1 * t1) / t1 === t1
      20.toDouble / t1 === Array[Double](5, 4).asRow
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
    "transpose row vector" in {
      1.constTensor(4).t === Array(1, 1, 1, 1).asCol
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
      d.shape === Seq(3, 3)
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
    "reshape" in {
      val data = Array(1, 2)
      val x = data.asRow
      val in = x.reshape(2, 1)
      in === Array(1, 2).asCol
      an[IllegalArgumentException] should be thrownBy x.reshape(5)
      x.asCol() === Array(1, 2).asCol
      x.asCol().asRow() === Array(1, 2).asRow
      in.isView === true
      in(0) = 5
      in(0) === 5
    }
    "drop singular dimension" in {
      val t = Array[Int](
        1, 2, 3)
        .asTensor(1, 3, 1)
      t.dropSingular(2).shape === Seq(1, 3)
      an[IllegalArgumentException] should be thrownBy t.dropSingular(1)
    }
    "add singular dimension" in {
      val t = Array(1, 2, 3, 4).asCols(2).addSingular(1)
      t.shape === Seq(2, 1, 2)
    }
    "Indexing tensor" in {
      val shape = Vector(2, 3, 4)
      val t = IndexTensor(shape)
      t.length === shape.product
    }
    "dissect tensor" in {
      val tensor = Array(
        0, 1,
        2, 3,
        4, 5,

        6, 7,
        8, 9,
        10, 11)
        .asTensor(2, 3, 2)
      val expected: Tensor[Tensor[Int]] =
        Array[Tensor[Int]](
          Array(
            0, 1,
            2, 3,
            4, 5)
            .asTensor(1, 3, 2),
          Array(
            6, 7,
            8, 9,
            10, 11)
            .asTensor(1, 3, 2))
          .asTensor(2, 1, 1)
      val expected2: Tensor[Tensor[Int]] =
        Array[Tensor[Int]](
          Array(
            0,
            2,
            4,

            6,
            8,
            10)
            .asTensor(2, 3, 1),
          Array(
            1,
            3,
            5,

            7,
            9,
            11)
            .asTensor(2, 3, 1))
          .asTensor(1, 1, 2)

      val a1 = Array(0, 2, 4).asTensor(1, 3, 1)
      val a2 = Array(1, 3, 5).asTensor(1, 3, 1)
      val a3 = Array(6, 8, 10).asTensor(1, 3, 1)
      val a5 = Array(7, 9, 11).asTensor(1, 3, 1)
      val expected3 = Array[Tensor[Int]](a1, a2, a3, a5).asTensor(2, 1, 2)
      tensor.dissect(1, 2) === expected
      tensor.dissect(0, 1) === expected2
      tensor.dissect(1) === expected3
    }
    "concatenate tensors along one dimension" in {
      val a = Array[Int](
        0, 1,
        2, 3)
        .asTensor(2, 2)
      val b = Array[Int](
        4, 5)
        .asTensor(1, 2)
      val result = Array(a, b).asCol
      val exp = Array[Int](
        0, 1,
        2, 3,
        4, 5)
        .asRows(3)
      result.concat() === exp
    }
    "get data" in {
      val values = Array(1, 2, 3, 4)
      values.asRows(2).getData.sameElements(values)
    }
  }
  "inflate from long to int should" - {
    "produce correct values" in {
      implicit val c = new LongToIntConverter

      val tensor = Array[Long](1, 2, 3, 4)
        .asRow
        .inflate[Int]

      tensor.sameElements(Seq(0, 1, 0, 2, 0, 3, 0, 4))
    }
  }
  "deflate should" - {
    "deflate" in {
      implicit val c: LongToIntConverter = new LongToIntConverter
      def rand = Random.nextLong()
      val back = Array[Long](rand, rand, rand)
      val tmp = back.asRow.inflate[Int].cached
      val tensor = tmp.deflate[Long]
      val long = tensor
      long.sameElements(back)
    }
    "throw exception if size is not compatible with type" in {
      import convert.implicits.longToInt
      an[IllegalArgumentException] should be thrownBy Array(0).asRow.deflate[Long]
    }
  }
}
