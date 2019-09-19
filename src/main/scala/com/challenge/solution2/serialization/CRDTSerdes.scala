package com.challenge.solution2.serialization
import com.google.protobuf.any.{Any => ProtoAny}

trait CRDTSerdes[E] {

  def serialize(e: E)(implicit converter: CRDTSerdes[E]): ProtoAny

  def deserialize(proto: ProtoAny)(implicit converter: CRDTSerdes[E]): E

}
