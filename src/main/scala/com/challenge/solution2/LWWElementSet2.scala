package com.challenge.solution2

/*
 * an element of type E put into this set be immutable. It should also implement a good hashcode function to increase performance.
 * This implementation is not very functional, it is better to use IO container type if we want more functional style
 */
case class LWWElementSet2[E](addSet: TimestampGSet[E], removeSet: TimestampGSet[E])(clock: LWWElementSetClock) {
  //remove: O(1) time
  //no need to throw exceptions if the element is not in the set
  def remove(ele: E): LWWElementSet2[E] = {
    if (lookup(ele)) {
      copy(removeSet = removeSet.add(ele, clock.now()))(clock)
    } else {
      this
    }
  }

  //add: O(1) time
  def add(ele: E): LWWElementSet2[E] = copy(addSet = addSet.add(ele, clock.now()))(clock)

  //lookup: amortized O(1) time
  def lookup(ele: E): Boolean = {
    addSet.latestTimestampBy(ele)
      .exists { addTs =>
        removeSet.latestTimestampBy(ele) match {
          case Some(removeTs) => addTs.compareTo(removeTs) > 0 //bias towards removal in case of same timestamps
          case None => true
        }
      }
  }

  //merge: O(#SumOfElementsInTwoSets) time
  def merge(other: LWWElementSet2[E]): LWWElementSet2[E] = copy(addSet.merge(other.addSet), removeSet.merge(other.removeSet))(clock)

  //toSet: O(#elements in addSet) time
  def toSet: Set[E] = addSet.entries.foldLeft(List[E]()) {
    case (acc, (ele, latestAddTs)) => {
      removeSet.latestTimestampBy(ele) match {
        case Some(latestRemoveTs) if latestAddTs.compareTo(latestRemoveTs) > 0 => ele :: acc
        case Some(_) => acc
        case None => ele :: acc
      }
    }
  }.toSet

  def compare(that: LWWElementSet2[E]): Boolean = {
    addSet.compare(that.addSet) && removeSet.compare(that.removeSet)
  }
}

object LWWElementSet2 {
  def empty[E](clock: LWWElementSetClock = new LWWElementSetClockImpl()): LWWElementSet2[E] = LWWElementSet2(TimestampGSet[E](), TimestampGSet[E]())(clock)

  def from[E](es: Seq[E], clock: LWWElementSetClock = new LWWElementSetClockImpl()): LWWElementSet2[E] = {
    es.foldRight(empty[E](clock)) {
      case (ele, accumulate) => accumulate.add(ele)
    }
  }
}
