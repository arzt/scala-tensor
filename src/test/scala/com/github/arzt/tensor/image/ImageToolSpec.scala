package com.github.arzt.tensor
package image

import java.awt.image.BufferedImage
import java.awt.image.DataBufferByte

import com.github.arzt.tensor.TensorImplicits._
import org.specs2.mutable.Specification

import scala.util.Random.nextBytes

class ImageToolSpec extends Specification {
  "ImageTool" should {
    val width = 4
    val height = 3
    val bytes = new Array[Byte](width * height * 3)
    nextBytes(bytes)

    "convert image to tensor" in {
      val image = new BufferedImage(width, height, BufferedImage.TYPE_3BYTE_BGR)
      val data = image.getRaster.getDataBuffer.asInstanceOf[DataBufferByte].getBankData()(0)
      Array.copy(bytes, 0, data, 0, bytes.length)
      val tensor = image.asTensor
      tensor.shape === Seq(height, width, 3)
      tensor.sameElements(bytes)
    }
    "covert tensor to image" in {
      val image = Tensor(data = bytes, height, width, 3).asImage(BufferedImage.TYPE_3BYTE_BGR)
      image.getWidth === width
      image.getHeight === height
      image
        .getData
        .getDataBuffer
        .asInstanceOf[DataBufferByte]
        .getBankData()(0)
        .sameElements(bytes)
    }
  }
}
