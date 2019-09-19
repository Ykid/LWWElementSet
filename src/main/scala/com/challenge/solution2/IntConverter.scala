package com.challenge.solution2

import com.google.protobuf.wrappers.Int32Value
import com.google.protobuf.any.{Any => ProtoAny}

class IntConverter extends CRDTSerdes[Int] {
  import IntConverter._

  override def serialize(e: Int)(implicit converter: CRDTSerdes[Int]): ProtoAny = {
    val protoInt = Int32Value(e)
    ProtoAny.of(url, protoInt.toByteString)
  }

  override def deserialize(proto: ProtoAny)(implicit converter: CRDTSerdes[Int]): Int = {
    if (proto.typeUrl != url) throw new Exception("type url not match!")
    Int32Value.parseFrom(proto.value.toByteArray).value
  }
}

object IntConverter {
  implicit val defaultCoverter = new IntConverter()
  val url = "type.googleapis.com/scala.Int"
}