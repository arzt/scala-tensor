package com.github.arzt.tensor.convert

import org.specs2.mutable.Specification

import scala.util.Random

class ConverterTest extends Specification {
  "long int converter" should {
    "test endianess" in {
      val c = new LongToIntConverter
      val r = new Random()
      val a = r.nextInt()
      val result = c.write(0L, 1, a)
      result.toInt === a
    }
    "convert long to ints and vice versa" in {
      val c = new LongToIntConverter
      val r = new Random()
      val a = r.nextInt()
      val b = r.nextInt()
      val l = c.write(0L, 0, a)
      val result = c.write(l, 1, b)
      c.read(result, 0) shouldEqual a
      c.read(result, 1) shouldEqual b
    }
  }
  "long byte converter" should {
    "convert long to bytes and vice versa" in {
      val converter = new LongToByteConverter
      val r = new Random()
      Range(0, 8)
        .indices
        .map { i =>
          val randLong = r.nextLong()
          val value = r.nextBytes(1)(0)
          val written = converter.write(randLong, i, value)
          converter.read(written, i) shouldEqual value
        }
    }
  }
  "int byte converter" should {
    "converts int to bytes and vice versa" in {
      val converter = new IntToByteConverter
      val r = new Random(5)
      Range(0, converter.n)
        .indices
        .map { i =>
          val source = r.nextInt()
          val target = r.nextBytes(1)(0)
          val written = converter.write(source, i, target)
          val readd = converter.read(written, i)
          readd shouldEqual target
        }
    }
  }
  "byte boolean converter" should {
    "converts byte to booleans and vice versa" in {
      val converter = new ByteToBooleanConverter
      val r = new Random(4)
      Range(0, 1)
        .indices
        .map { i =>
          val source = r.nextBytes(1)(0)
          val value = r.nextInt() % 2 != 0
          val written = converter.write(source, i, value)
          val result = converter.read(written, i)
          result shouldEqual value
        }
    }
  }
}
