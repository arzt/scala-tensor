package com.github.arzt.tensor
package image

import java.awt.image.BufferedImage
import java.awt.image.DataBuffer
import java.awt.image.DataBufferByte
import java.lang.Double.longBitsToDouble

object ImageTool {

  def dataBufferToBytes(buffer: DataBuffer): Tensor[Byte] = {
    buffer match {
      case bb: DataBufferByte =>
        val data = bb.getBankData()(0)
        new ArrayTensor[Byte](Vector(bb.getSize), data)
      case _ =>
        import convert.implicits._
        val data = (0 until buffer.getSize).map(buffer.getElemDouble).toArray
        new ArrayTensor[Double](Vector(data.length), data)
          .map(java.lang.Double.doubleToLongBits)
          .inflate[Byte]
    }
  }

  def bytesToDataBuffer(buffer: DataBuffer, tensor: Tensor[Byte]): DataBuffer = {
    buffer match {
      case bb: DataBufferByte =>
        val data = bb.getBankData()(0)
        tensor.fillArray(data, 0)
        buffer
      case _ =>
        import convert.implicits._
        val values = tensor.deflate[Long].map(longBitsToDouble)
        var i = 0
        val valueIterator = values.toIterable.iterator
        while (i < buffer.getSize) {
          buffer.setElemDouble(i, valueIterator.next())
          i += 1
        }
        buffer
    }
  }

  def fromImage(image: BufferedImage): Tensor[Byte] = {
    val buffer = image.getRaster.getDataBuffer
    dataBufferToBytes(buffer)
      .reshape(
        image.getHeight,
        image.getWidth,
        image.getColorModel.getNumComponents)
  }

  def writeImage(image: BufferedImage, tensor: Tensor[Byte]): BufferedImage = {
    bytesToDataBuffer(image.getRaster.getDataBuffer, tensor)
    image
  }

  def toImage(tensor: Tensor[Byte], imageType: Int): BufferedImage = {
    val width = tensor.shape(1)
    val height = tensor.shape(0)
    val image = new BufferedImage(width, height, imageType)
    writeImage(image, tensor)
    image
  }

}