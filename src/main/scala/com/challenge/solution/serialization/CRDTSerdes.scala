package com.challenge.solution.serialization

import com.google.protobuf.any.{Any => ProtoAny}

trait CRDTSerdes[E] {

  def serialize(e: E): ProtoAny

  def deserialize(proto: ProtoAny): E

}
