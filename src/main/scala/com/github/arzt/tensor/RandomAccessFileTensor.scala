package com.github.arzt.tensor

import java.io.RandomAccessFile
import java.nio.channels.FileChannel
import collection.immutable.Seq

class RandomAccessFileTensor(raf: RandomAccessFile) extends Tensor[Byte] {

  override def shape: Seq[Int] = List(raf.length().toInt)

  private val mapped = raf.getChannel.map(FileChannel.MapMode.READ_ONLY, 0, raf.length())

  override def isView: Boolean = true

  override def apply(a: Int): Byte =
    mapped.get(a)

  override def update(a: Int, v: Byte): Unit =
    mapped.put(a, v)
}
