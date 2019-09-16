package com.challenge

import java.time.{Clock, Instant}

//this might not be in the most strict sense a Gset because the same element of different timestamps are treated as different in this set
class GSet(copy: Set[(Int, Instant)] = Set(), clock: Clock = Clock.systemUTC()) {
  val internalSet: Set[(Int, Instant)] = copy

  //prevent duplicate add ?
  //by definition the a set can only add items and cannot remove items
  def add(elem: Int): GSet = {
    /* what if the same element has been added again and again ? But it is a set, not a list, there should be no effect
    *  if the elements are added repeatedly
    * */
    val tp = (elem, clock.instant())
    new GSet(internalSet + tp)
  }

  def lookup(elem: Int): Boolean = internalSet.exists {
    case (e, _) => e == elem
  }

  def merge(another: GSet): GSet = new GSet(internalSet.union(another.internalSet))

  def getLatest(ele: Int): Option[(Int, Instant)] = {
    internalSet.foldLeft(None: Option[(Int, Instant)]) {
      case (accumulated@Some((prevE, prevTimestamp)), current@(e, timestamp)) => {
        //same element, but the timestamp is newer
        if (prevE == e && prevTimestamp.compareTo(timestamp) <= 0) {
          Some(current)
        } else {
          accumulated
        }
      }
      case (None, current@(e, _)) =>
        if (ele == e) Some(current) else None
    }
  }

  override def toString: String = s"${internalSet.toString}"
}
