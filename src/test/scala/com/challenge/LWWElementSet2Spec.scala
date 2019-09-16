package com.challenge

import java.time.{Clock, Instant, ZoneId}

import org.scalatest.{FunSpec, Matchers}

class LWWElementSet2Spec extends FunSpec with Matchers {


  /*
   * TODO: property base check ?
   */
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
        val mockClock = Clock.fixed(Instant.now(), ZoneId.of(ZoneId.SHORT_IDS.get("CTT")))
        val set = LWWElementSet2()(mockClock)
        set.add(1).remove(1).toSet should ===(Set())
      }
    }

  }
}
