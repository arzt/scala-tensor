package com.github.arzt.tensor.convert

package object implicits {

  implicit val byteToBoolean: Converter[Byte, Boolean] = new ByteToBooleanConverter

  implicit val intToBoolean: Converter[Int, Boolean] = new IntToBooleanConverter

  implicit val intToByte: Converter[Int, Byte] = new IntToByteConverter

  implicit val intToShort: Converter[Int, Short] = new IntToShortConverter

  implicit val longToBoolean: Converter[Long, Boolean] = new LongToBooleanConverter

  implicit val longToByte: Converter[Long, Byte] = new LongToByteConverter

  implicit val longToInt: Converter[Long, Int] = new LongToIntConverter

  implicit val longToShort: Converter[Long, Short] = new LongToShortConverter

  implicit val shortToBoolean: Converter[Short, Boolean] = new ShortToBooleanConverter

  implicit val shortToByte: Converter[Short, Byte] = new ShortToByteConverter

}
