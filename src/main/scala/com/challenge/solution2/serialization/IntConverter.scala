package com.challenge.solution2.serialization

import com.google.protobuf.any.{Any => ProtoAny}
import com.google.protobuf.wrappers.Int32Value

class IntConverter() extends CRDTSerdes[Int] {

  import IntConverter._

  override def serialize(e: Int)(implicit converter: CRDTSerdes[Int]): ProtoAny = {
    val protoInt = Int32Value(e)
    ProtoAny.of(url, protoInt.toByteString)
  }

  override def deserialize(proto: ProtoAny)(implicit converter: CRDTSerdes[Int]): Int = {
    if (proto.typeUrl != url) throw SerializationException(s"type url not match!, expected: $url, received ${proto.typeUrl}")
    Int32Value.parseFrom(proto.value.toByteArray).value
  }
}

object IntConverter {
  implicit val defaultCoverter = new IntConverter()
  val url = "type.googleapis.com/scala.Int"
}