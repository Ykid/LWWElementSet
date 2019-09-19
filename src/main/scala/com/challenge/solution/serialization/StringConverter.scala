package com.challenge.solution.serialization

import com.google.protobuf.any
import com.google.protobuf.any.{Any => ProtoAny}
import com.google.protobuf.wrappers.StringValue

class StringConverter() extends CRDTSerdes[String] {

  import StringConverter.url

  override def serialize(e: String)(implicit converter: CRDTSerdes[String]): any.Any = {
    val protoStr = StringValue(e)
    ProtoAny.of(url, protoStr.toByteString)
  }

  override def deserialize(proto: any.Any)(implicit converter: CRDTSerdes[String]): String = {
    if (proto.typeUrl != url)
      throw SerializationException(s"type url not match!, expected: $url, received ${proto.typeUrl}")
    StringValue.parseFrom(proto.value.toByteArray).value
  }
}

object StringConverter {
  val url = "type.googleapis.com/java.lang.String"
  implicit val defaultCoverter = new StringConverter()

}
