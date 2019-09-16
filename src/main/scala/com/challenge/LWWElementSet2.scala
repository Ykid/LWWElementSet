package com.challenge

import java.time.{Clock, Instant}

import com.challenge.LWWElementSet2.GSet2

import scala.collection.immutable.HashMap

case class LWWElementSet2(addSet: GSet2 = GSet2(), removeSet: GSet2 = GSet2())(clock: Clock = Clock.systemUTC()) {

  //objective: add an efficient implementation of LWW Set
  //remove: amortized O(1) time
  //simply append the item to the end

  //add: amortized O(1) time
  //simply append the item to the end

  //lookup: amortized O(1) time
  //hash map related implementation

  //merge: O(#SumOfElementsInTwoSets) time
  //does not depends on lookup. merge the two subsets

  //toSet: O(#elements in addSet) time

  def remove(ele: Int): LWWElementSet2 = {
    if (lookup(ele)) {
      copy(removeSet = removeSet.add(ele, clock.instant()))(clock)
    } else {
      this
    }
  }

  def add(ele: Int): LWWElementSet2 = copy(addSet = addSet.add(ele, clock.instant()))(clock)

  def lookup(ele: Int): Boolean = {
    addSet.latestTimestampBy(ele)
      .exists { addTs =>
        removeSet.latestTimestampBy(ele) match {
          case Some(removeTs) => addTs.compareTo(removeTs) > 0
          case None => true
        }
      }
  }

  def merge(other: LWWElementSet2): LWWElementSet2 = copy(addSet.merge(other.addSet), removeSet.merge(other.removeSet))(clock)

  def toSet: Set[Int] = addSet.entries.foldLeft(List[Int]()) {
    case (acc, (ele, latestAddTs :: _)) => {
      removeSet.latestTimestampBy(ele) match {
        case Some(latestRemoveTs) if latestAddTs.compareTo(latestRemoveTs) > 0 => ele :: acc
        case Some(_) => acc
        case None => ele :: acc
      }
    }
  }.toSet
}

object LWWElementSet2 {
  type Element = Int

  case class Entry(element: Element, timestamp: Instant)

  case class GSet2(entries: HashMap[Element, List[Instant]] = HashMap()) {
    def merge(that: GSet2): GSet2 = {
      val updated = entries.merged(that.entries) {
        //elements are equal, so ignores the other
        case ((element, thisTimestamps@thisLastUpdated :: _), (_, thatTimeStamps@thatLastUpdated :: _)) => {
          if (thisLastUpdated.compareTo(thatLastUpdated) <= 0) {
            (element, thatTimeStamps.concat(thisTimestamps))
          } else {
            (element, thisTimestamps.concat(thatTimeStamps))
          }
        }
      }
      copy(entries = updated)
    }

    def add(element: Element, timestamp: Instant): GSet2 = {
      val updated = entries.updatedWith(element) {
        case Some(timestamps) => Some(timestamp :: timestamps)
        case None => Some(List(timestamp))
      }
      copy(entries = updated)
    }

    def lookup(element: Element): Boolean = entries.contains(element)

    //if the element does not exist, returns none, otherwise returns the latest timestamp
    def latestTimestampBy(element: Element): Option[Instant] = entries.get(element) match {
      case Some(head :: _) => Some(head)
      case _ => None
    }
  }

}
