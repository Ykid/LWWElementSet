package com.challenge

import java.time.Instant

object Main extends App {
  val lwwSet = new LWWElementSet()
  println(lwwSet.add(1))
  println(lwwSet.add(1).add(1))
  println(lwwSet.add(1).add(1).add(1))
}

//assume it si a set of numbers for simplicity
class LWWElementSet(private val addSet: GSet = new GSet(), private val removeSet: GSet = new GSet()) {
  //2 G sets
  //add set
  //remove set

  def add(elem: Int): LWWElementSet = {
    new LWWElementSet(addSet.add(elem), removeSet)
  }

  def remove(elem: Int): LWWElementSet = {
    if (lookup(elem)) {
      new LWWElementSet(addSet, removeSet.add(elem))
    } else {
      //can also throw exceptions
      this
    }
  }

  def lookup(elem: Int): Boolean = {
    addSet.getLatest(elem) match {
      case Some((_, ts)) => //the element exists
        removeSet.getLatest(elem) match {
          //add bias: if the timestamps are equal, it is treated as added
          case Some((_, ts2)) => ts2.compareTo(ts) < 0
          case None => true
        }
      case None => false
    }
  }

  def toSet: Set[Int] = {
    //get entries for the set
  }
}

class GSet(copy: Set[(Int, Instant)] = Set()) {
  private val internalSet: Set[(Int, Instant)] = copy

  //prevent duplicate add ?
  //by definition the a set can only add items and cannot remove items
  def add(elem: Int): GSet = {
    /* what if the same element has been added again and again ? But it is a set, not a list, there should be no effect
    *  if the elements are added repeatedly
    * */
    val tp = (elem, Instant.now())
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
