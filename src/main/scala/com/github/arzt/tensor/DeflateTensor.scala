package com.github.arzt.tensor

import com.github.arzt.tensor.convert.Converter

import scala.collection.immutable.Seq

class DeflateTensor[T, U](val shape: Seq[Int], val parent: Tensor[U])(implicit val convert: Converter[T, U]) extends Tensor[T] {

  override def isView: Boolean = parent.isView

  override def apply(i: Int): T = {
    (0 until convert.n)
      .foldLeft(convert.zero) {
        (s, j) =>
          {
            val p = parent(i * convert.n + j)
            convert.write(s, j, p)
          }
      }
  }

  override def update(i: Int, v: T): Unit = {
    Range(0, convert.n)
      .foreach {
        j =>
          {
            parent.update(i * convert.n + j, convert.read(v, j))
          }
      }
  }

}
