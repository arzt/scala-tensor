package com.github.arzt.tensor
package image

import com.github.arzt.tensor.TensorImplicits._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import java.awt.image.BufferedImage.TYPE_3BYTE_BGR
import java.awt.image.BufferedImage.TYPE_INT_ARGB
import scala.util.Random.nextBytes

class ImageToolSpec extends AnyFreeSpec with Matchers {
  "ImageTool should" - {
    val width = 4
    val height = 3
    val bytes = new Array[Byte](width * height * 4)
    nextBytes(bytes)

    "convert tensor to image (3 byte type) and back" in {
      val value1 = bytes.asTensor(height, width, 4)
      val source = value1(All, All, 1 :: -1)
      val image = source.asImage(TYPE_3BYTE_BGR)
      val target = image.asTensor
      image.getWidth === width
      image.getHeight === height
      source.sameElements(target) === true
    }
    "convert tensor to image (4 byte type) and back" in {
      val source = bytes.asTensor(height, width, 4)
      source.asImage(TYPE_INT_ARGB).asTensor.sameElements(source) === true
    }
  }
}
