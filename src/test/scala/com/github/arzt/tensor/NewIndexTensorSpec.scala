package com.github.arzt.tensor

import com.github.arzt.tensor.NewIndexTensor.unindex
import org.specs2.mutable.Specification

class NewIndexTensorSpec extends Specification {
  "Grid tensor" should {
    "comptute sth " in {
      val indexTensor = NewIndexTensor(2, 3)
      indexTensor.shape === Vector(2, 3, 2)
      val expected = Seq(
        0, 0, 0, 1, 0, 2,
        1, 0, 1, 1, 1, 2)
      indexTensor.toIterable.toSeq === expected
      indexTensor.sameElements(expected) === true
    }
  }
  "unindex" should {
    "reverse" in {
      NewIndexTensor.unindex(List(1), 0, 0) === 0
    }
    "reverse indexing" in skipped {
      NewIndexTensor.unindex(List(2, 3), 0, 0) === 0
      NewIndexTensor.unindex(List(2, 3), 0, 1) === 0
      NewIndexTensor.unindex(List(2, 3), 1, 0) === 0
      NewIndexTensor.unindex(List(2, 3), 1, 1) === 1
      NewIndexTensor.unindex(List(2, 3), 2, 0) === 0
      NewIndexTensor.unindex(List(2, 3), 2, 1) === 2
      NewIndexTensor.unindex(List(2, 3), 3, 0) === 1
      NewIndexTensor.unindex(List(2, 3), 3, 1) === 0
      NewIndexTensor.unindex(List(2, 3), 4, 0) === 1
      NewIndexTensor.unindex(List(2, 3), 4, 1) === 1
    }
  }
}
