package com.challenge.solution

import com.challenge.solution.proto.lwwelementset.{LWWElementSet => LWWElementSetProto}
import com.challenge.solution.serialization.{CRDTSerdes, SerializationException}

import scala.util.Try

/*
 * An element of type E put into this set be immutable. It should also implement a good hashcode function to increase performance.
 * This implementation is not very functional, it is better to use IO container type if we want more functional style
 */
case class LWWElementSet[E](addSet: TimestampGSet[E], removeSet: TimestampGSet[E])(clock: LWWElementSetClock) {
  require(addSet != null)
  require(removeSet != null)
  require(clock != null)

  def remove(ele: E): LWWElementSet[E] = {
    require(ele != null)
    if (query(ele)) {
      copy(removeSet = removeSet.add(ele, clock.now()))(clock)
    } else {
      this
    }
  }

  def add(ele: E): LWWElementSet[E] = {
    require(ele != null)
    copy(addSet = addSet.add(ele, clock.now()))(clock)
  }

  def query(ele: E): Boolean =
    addSet
      .latestTimestampBy(ele)
      .exists { addTs =>
        removeSet.latestTimestampBy(ele) match {
          case Some(removeTs) => addTs.compareTo(removeTs) > 0 //bias towards removal in case of same timestamps
          case None           => true
        }
      }

  def merge(that: LWWElementSet[E]): LWWElementSet[E] = {
    require(that != null)
    copy(addSet.merge(that.addSet), removeSet.merge(that.removeSet))(clock)
  }

  def toSet: Set[E] =
    addSet.entries
      .foldLeft(List[E]()) {
        case (acc, (ele, latestAddTs)) =>
          removeSet.latestTimestampBy(ele) match {
            case Some(latestRemoveTs) if latestAddTs.compareTo(latestRemoveTs) > 0 => ele :: acc
            case Some(_)                                                           => acc
            case None                                                              => ele :: acc
          }
      }
      .toSet

  def compare(that: LWWElementSet[E]): Boolean = {
    require(that != null)
    addSet.compare(that.addSet) && removeSet.compare(that.removeSet)
  }
}

object LWWElementSet {
  def empty[E](clock: LWWElementSetClock = new LWWElementSetClockImpl()): LWWElementSet[E] =
    LWWElementSet(TimestampGSet[E](), TimestampGSet[E]())(clock)

  def from[E](es: Seq[E], clock: LWWElementSetClock = new LWWElementSetClockImpl()): LWWElementSet[E] =
    es.foldRight(empty[E](clock)) {
      case (ele, accumulate) => accumulate.add(ele)
    }

  //for data transmission between nodes
  def serialize[E](set: LWWElementSet[E])(implicit converter: CRDTSerdes[E]): LWWElementSetProto =
    LWWElementSetProto(Some(TimestampGSet.serialize(set.addSet)), Some(TimestampGSet.serialize(set.removeSet)))

  def deserialize[E](proto: LWWElementSetProto)(implicit converter: CRDTSerdes[E]): Try[LWWElementSet[E]] =
    Try {
      proto match {
        case LWWElementSetProto(Some(protoAddSet), Some(protoRemoveSet)) =>
          val addSet = TimestampGSet.deserialize(protoAddSet).get
          val removeSet = TimestampGSet.deserialize(protoRemoveSet).get
          val valid = removeSet.entries.forall {
            case (e, _) => addSet.latestTimestampBy(e).nonEmpty
          }
          if (!valid)
            throw SerializationException(
              s"Invalid format: some elements are in removeSet but not addSet, impossible. addSet: $addSet, removeSet: $removeSet"
            )
          LWWElementSet(addSet, removeSet)(new LWWElementSetClockImpl())
        case _ =>
          throw SerializationException(
            s"Invalid format: not both addSet or removeSet are defined. proto: ${proto.toProtoString}"
          )
      }
    }
}
