package com.challenge.solution2

import java.time.Instant

import scala.collection.immutable.HashMap
import com.challenge.solution2.proto.lwwelementset.{TimestampGSet => TimestampGSetProto}
import com.google.protobuf.timestamp.Timestamp

import scala.util.Try

/*
 * a mutated version of Growth set
 * - only latest timestamp is kept, others are dropped for time and space efficiency
 * - element should be added with a timestamp
 */
case class TimestampGSet[E](entries: HashMap[E, Instant] = HashMap[E, Instant]()) {
  require(entries != null)

  def merge(that: TimestampGSet[E]): TimestampGSet[E] = {
    require(that != null)
    val updated = entries.merged(that.entries) {
      case ((element, thisLastUpdated), (_, thatLastUpdated)) =>
        (element, max(thisLastUpdated, thatLastUpdated))
    }
    copy(entries = updated)
  }

  def add(element: E, timestamp: Instant): TimestampGSet[E] = {
    require(element != null)
    require(timestamp != null)

    val updated = entries.updatedWith(element) {
      case Some(existing) => Some(max(existing, timestamp))
      case None => Some(timestamp)
    }
    copy(entries = updated)
  }

  def latestTimestampBy(element: E): Option[Instant] = entries.get(element)

  def toSet: Set[E] = entries.keySet

  def compare(that: TimestampGSet[E]): Boolean = {
    require(that != null)
    /*
     * denote the elements of this class as elem(A), elem(A) = keys(entries)
     * denote the associated timestamp of element e in this set A as ts(e, A)
     * (1) elem(this) is a subset of elem(that)
     * (2) for all e in (intersection of elem(this) and elem(that)), ts(e, this) <= ts(e, that)
     *
     * returns true if (1) and (2) are met and false otherwise
     */
    val thisElems = entries.keySet
    val thatElems = that.entries.keySet
    val cond1 = thisElems.subsetOf(thatElems)

    lazy val cond2 = thisElems.forall { elem =>
      that.entries.get(elem) match {
        case Some(ts) => entries(elem).compareTo(ts) <= 0
        case None => true
      }
    }
    cond1 && cond2
  }

  private def max(ts1: Instant, ts2: Instant): Instant = if (ts1.compareTo(ts2) <= 0) ts2 else ts1
}

object TimestampGSet {

  import TimestampGSetProto._

  def serialize[E](set: TimestampGSet[E])(implicit converter: CRDTSerdes[E]): TimestampGSetProto = {
    val protoEntries = set.entries.toSeq.map {
      case (k, v) => Entry(Some(converter.serialize(k)), Some(Timestamp(v.getEpochSecond, v.getNano)))
    }
    TimestampGSetProto(protoEntries)
  }

  def deserialize[E](proto: TimestampGSetProto)(implicit converter: CRDTSerdes[E]): Try[TimestampGSet[E]] = {
    Try {
      val tuples: Seq[(E, Instant)] = proto
        .entries
        .flatMap {
          case Entry(Some(k), Some(v)) => Some((converter.deserialize(k), Instant.ofEpochSecond(v.seconds, v.nanos)))
          case _ => None
        }
      TimestampGSet(HashMap.from(tuples))
    }
  }
}
