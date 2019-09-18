package com.challenge

import java.time.{Clock, Instant, ZoneId}

import org.scalatest.{FunSpec, Matchers}

class LWWElementSet2Spec extends FunSpec with Matchers with TimeMeasurementHelper {

  describe("A LWWElementSet") {
    describe("query functionality") {
      it("should report true if the element in question in the set") {
        val set = LWWElementSet2()()
        set.add(1).lookup(1) should be(true)
      }

      it("should report false if the element in question in the set") {
        val set = LWWElementSet2()()
        set.lookup(1) should be(false)
      }
    }

    describe("basic add functionality") {
      it("should be able to add one element when empty") {
        val set = LWWElementSet2()()
        set.add(1).toSet should ===(Set(1))
      }

      it("should add duplicate element only once") {
        val set = LWWElementSet2()()
        set.add(1).add(1).toSet should ===(Set(1))
      }

      it("should be able to add different elements") {
        val set = LWWElementSet2()()
        set.add(1).add(2).add(3).toSet should ===(Set(1, 2, 3))
      }
    }

    describe("basic remove functionality") {
      it("should be able remove element if the set contains it") {
        val set = LWWElementSet2()()
        set.add(1).remove(1).toSet should ===(Set())
      }

      it("should do nothing it the element to be removed is not in the set") {
        val set = LWWElementSet2()()
        set.add(1).remove(2).toSet should ===(Set(1))
      }
    }

    describe("merge functionality") {
      it("case one: different elements will appear in the merge set") {
        val set1 = LWWElementSet2()().add(1)
        val set2 = LWWElementSet2()().add(2)
        set1.merge(set2).toSet should ===(Set(1, 2))
      }

      it("case two: same elements will not be duplicated") {
        val set1 = LWWElementSet2()().add(1)
        val set2 = LWWElementSet2()().add(1)
        set1.merge(set2).toSet should ===(Set(1))
      }

      it("case three: if element in set1 is deleted in the other set, the element will not shown") {
        val set1 = LWWElementSet2()().add(1)
        Thread.sleep(5)
        val set2 = LWWElementSet2()().add(1).remove(1)
        set1.merge(set2).toSet should ===(Set())
      }

      it("case four: if deleted element in set1 is added back in the other set, the element will be shown") {
        val set1 = LWWElementSet2()().add(1).remove(1)
        Thread.sleep(5)
        val set2 = LWWElementSet2()().add(1)
        set1.merge(set2).toSet should ===(Set(1))
      }

      it("case five: if the latest add transaction has the same timestamp as the latest remove transaction, the element is treated as removed") {
        val fixedTimestampClock: LWWElementSetClock = () => Instant.ofEpochMilli(10000)
        val set = LWWElementSet2()(fixedTimestampClock)
        set.add(1).remove(1).toSet should ===(Set())
      }
    }

    describe("performance") {
      it("add 1,000,000 elements and then lookup") {
        val createdSet = time {
          (1 to 1_000_000_0).foldLeft(LWWElementSet2()()) {
            case (lwwSet, ele) => lwwSet.add(ele)
          }
        }
        val result = time {
          createdSet.lookup(-1)
        }
        result should be(false)
      }
    }

    describe("merge properties") {
      it("should be idempotent") {
        val set1 = LWWElementSet2()().add(1)
        set1.merge(set1).toSet should ===(set1.toSet)

        val set2 = LWWElementSet2()().add(1).remove(1)
        set2.merge(set2).toSet should ===(set2.toSet)
      }

      it("should be commutative") {
        val set1 = LWWElementSet2()().add(1).remove(1)
        val set2 = LWWElementSet2()().add(2).remove(2).add(3).add(1)
        set1.merge(set2).toSet should ===(set2.merge(set1).toSet)
      }

      it("should be associative") {
        val set1 = LWWElementSet2()().add(1).remove(1)
        Thread.sleep(5)
        val set2 = LWWElementSet2()().add(2).remove(2).add(3).add(1)
        Thread.sleep(5)
        val set3 = LWWElementSet2()().add(4).remove(4).add(1).add(3).remove(3)
        set1.merge(set2).merge(set3).toSet should ===(set1.merge(set2.merge(set3)).toSet)
      }
    }

  }
}
