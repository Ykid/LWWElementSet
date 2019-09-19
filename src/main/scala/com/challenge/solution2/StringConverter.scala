package com.challenge.solution2

import com.challenge.solution2.IntConverter.url
import com.google.protobuf.any
import com.google.protobuf.any.{Any => ProtoAny}
import com.google.protobuf.wrappers.StringValue

class StringConverter extends CRDTSerdes[String] {
  override def serialize(e: String)(implicit converter: CRDTSerdes[String]): any.Any = {
    val protoStr = StringValue(e)
    ProtoAny.of(url, protoStr.toByteString)
  }

  override def deserialize(proto: any.Any)(implicit converter: CRDTSerdes[String]): String = {
    if (proto.typeUrl != url) throw new Exception("type url not match!")
    StringValue.parseFrom(proto.value.toByteArray).value
  }
}

object StringConverter {
  val url = "type.googleapis.com/java.lang.String"
  implicit val defaultCoverter = new StringConverter()

}
