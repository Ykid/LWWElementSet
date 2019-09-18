package com.challenge.solution2

import java.time.Instant

import scala.collection.immutable.HashMap

//a mutated version of Growth set: only latest timestamp is kept, others are dropped for time and space efficiency
case class TimestampGSet[E](entries: HashMap[E, Instant] = HashMap[E, Instant]()) {
  def merge(that: TimestampGSet[E]): TimestampGSet[E] = {
    val updated = entries.merged(that.entries) {
      case ((element, thisLastUpdated), (_, thatLastUpdated)) =>
        (element, max(thisLastUpdated, thatLastUpdated))
    }
    copy(entries = updated)
  }

  def add(element: E, timestamp: Instant): TimestampGSet[E] = {
    val updated = entries.updatedWith(element) {
      case Some(existing) => Some(max(existing, timestamp))
      case None => Some(timestamp)
    }
    copy(entries = updated)
  }

  def latestTimestampBy(element: E): Option[Instant] = entries.get(element)

  //evaluate to true if `this` set is related to `that` set of Relation R, false otherwise
  def compare(that: TimestampGSet[E]): Boolean = {
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
