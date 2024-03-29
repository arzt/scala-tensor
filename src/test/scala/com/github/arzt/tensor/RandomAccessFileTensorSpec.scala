package com.github.arzt.tensor

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import java.io.RandomAccessFile
import java.nio.file.Files
import scala.util.Random

class RandomAccessFileTensorSpec extends AnyFreeSpec with Matchers {

  "Random access file tensor should" - {
    "read files as tensor" in {
      val tmpFile = Files.createTempFile("tmp-file", null)

      val writeFile = new RandomAccessFile(tmpFile.toString, "rw")
      val bytes = new Array[Byte](1024)
      Random.nextBytes(bytes)
      writeFile.write(bytes)
      writeFile.close()

      val raf = new RandomAccessFile(tmpFile.toString, "r")
      val tensor = new RandomAccessFileTensor(raf)
      tensor.length === bytes.length
      tensor.sameElements(bytes)
    }
  }
}
