package com.challenge

import java.time.{Clock, Instant}

import com.challenge.LWWElementSet2.LWWRegistrySet

import scala.collection.immutable.HashMap

trait LWWElementSetClock {
  def now(): Instant
}

class LWWElementSetClokImpl extends LWWElementSetClock {
  val clock: Clock = Clock.systemUTC()

  override def now(): Instant = clock.instant()
}

case class LWWElementSet2(addSet: LWWRegistrySet = LWWRegistrySet(), removeSet: LWWRegistrySet = LWWRegistrySet())(clock: LWWElementSetClock = new LWWElementSetClokImpl()) {

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

  //no need to throw exceptions if the element is not in the set
  def remove(ele: Int): LWWElementSet2 = {
    if (lookup(ele)) {
      copy(removeSet = removeSet.add(ele, clock.now()))(clock)
    } else {
      this
    }
  }

  def add(ele: Int): LWWElementSet2 = copy(addSet = addSet.add(ele, clock.now()))(clock)

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
    case (acc, (ele, latestAddTs)) => {
      removeSet.latestTimestampBy(ele) match {
        case Some(latestRemoveTs) if latestAddTs.compareTo(latestRemoveTs) > 0 => ele :: acc
        case Some(_) => acc
        case None => ele :: acc
      }
    }
  }.toSet

  //don't think too much, treat it as a set
  //can use merge to define it x <= x.merge(y), y <= y.merge(x), x.merge(y) = y.merge(x)

  def compare(that: LWWElementSet2): Boolean = {
    addSet.compare(that.addSet) && removeSet.compare(that.removeSet)
  }

}

object LWWElementSet2 {

  type Element = Int

  private def max(ts1: Instant, ts2: Instant): Instant = if (ts1.compareTo(ts2) <= 0) ts2 else ts1

  //we could use a GSet, but for efficiency reasons, since non latest timestamps are ineffect redundant,
  //these timestamps are dropped
  //TODO: maybe change it to TimestampGSet
  case class LWWRegistrySet(entries: HashMap[Element, Instant] = HashMap()) {
    def merge(that: LWWRegistrySet): LWWRegistrySet = {
      val updated = entries.merged(that.entries) {
        case ((element, thisLastUpdated), (_, thatLastUpdated)) =>
          (element, max(thisLastUpdated, thatLastUpdated))
      }
      copy(entries = updated)
    }

    def add(element: Element, timestamp: Instant): LWWRegistrySet = {
      val updated = entries.updatedWith(element) {
        case Some(existing) => Some(max(existing, timestamp))
        case None => Some(timestamp)
      }
      copy(entries = updated)
    }

    //if the element does not exist, returns none, otherwise returns the latest timestamp
    def latestTimestampBy(element: Element): Option[Instant] = entries.get(element)

    //true: this set in related to that set of Relation R, false otherwise
    def compare(that: LWWRegistrySet): Boolean = {
      /*
       * denote the elements of this class as elem(A), elem(A) = keys(entries)
       * denote the associated timestamp of element e in this set A as ts(e, A)
       * denote the max timestamp of this class is maxTs(A)
       * (1) elem(this) is a subset of elem(that)
       * (2) for all e in (intersection of elem(this) and elem(that)), ts(e, this) <= ts(e, that)
       *
       * returns true if (1) and (2) are met and false otherwise
       */
      val thisElems = entries.keySet
      val thatElems = that.entries.keySet
      val cond1 = thisElems.subsetOf(thatElems)

      lazy val cond2 = thisElems.intersect(thatElems).forall { commonElem =>
        (entries(commonElem) compareTo that.entries(commonElem)) <= 0
      }
      cond1 && cond2
    }
  }

}
