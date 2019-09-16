package com.challenge

//assume it si a set of numbers for simplicity
class LWWElementSet(private val addSet: GSet = new GSet(), private val removeSet: GSet = new GSet(), private var debug: Boolean = true) {
  /*
  * 2 sets that is like G set but it can do a little bit differently
  * 1. equality might a bit different, add(a) add(a) should result in two operations
  *
  * merge operations
  * - Merge the two sets of LWW Element Set
  * - maybe one can optimize so that it is fast to add element for each set and query whether the element exists
  *
  *
  * if all operations follows the rule and the internal structure is immutable, there is no need not sanity check of internal data structures
  *
  * TODO: some friendly apis maybe: add a bunch of elements
  */
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
          //bias towards removal: if the timestamps are equal, it is treated as removed
          case Some((_, ts2)) => ts2.compareTo(ts) < 0
          case None => true
        }
      case None => false
    }
  }

  def merge(other: LWWElementSet): LWWElementSet = {
    //intuitively it is merging the two addSet and RemoveSet respectively
    new LWWElementSet(addSet.merge(other.addSet), removeSet.merge(other.removeSet))
  }

  def toSet(): Set[Int] = {
    /*
     * return all the set entries
     * - get entries of the add set and check whether they are still in the LWW Set
     *  - for each entry, use the rule: lookup(e) = ∃ t, ∀ t 0 > t: (e,t) ∈ A ∧ (e,t0) / ∈ R)
     */
    addSet.internalSet.foldRight(Set[Int]()) {
      case ((element, _), acc) =>
        if (lookup(element)) acc + element else acc
    }
  }

  override def toString(): String = if (debug) s"addSet: ${addSet.toString} \nremoveSet: ${removeSet.toString}" else toSet().toString()

  def result(): Set[Int] = toSet()

}
