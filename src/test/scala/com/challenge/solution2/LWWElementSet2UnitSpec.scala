package com.challenge.solution2

import java.time.Instant

import com.challenge.TimeMeasurementHelper
import com.challenge.solution2.LWWElementSet2.{from, empty => emptySet}
import org.scalatest.{FunSpec, Matchers}

class LWWElementSet2UnitSpec extends FunSpec with Matchers with TestUtil {

  describe("A LWWElementSet") {
    describe("query functionality") {
      it("should report true if the element in question in the set") {
        val set = emptySet[Int]()
        set.add(1).lookup(1) should be(true)
      }

      it("should report false if the element in question in the set") {
        val set = emptySet[Int]()
        set.lookup(1) should be(false)
      }
    }

    describe("basic add functionality") {
      it("should be able to add one element when empty") {
        val set = emptySet[Int]()
        set.add(1).toSet should ===(Set(1))
      }

      it("should add duplicate element only once") {
        val set = emptySet[Int]()
        set.add(1).add(1).toSet should ===(Set(1))
      }

      it("should be able to add different elements") {
        val set = emptySet[Int]()
        set.add(1).add(2).toSet should ===(Set(1, 2))
      }
    }

    describe("basic remove functionality") {
      it("should be able remove element if the set contains it") {
        val set = emptySet[Int]()
        set.add(1).remove(1).toSet should ===(Set())
      }

      it("should do nothing it the element to be removed is not in the set") {
        val set = emptySet[Int]()
        set.add(1).remove(2).toSet should ===(Set(1))
      }
    }

    describe("merge functionality") {
      it("case one: different elements will appear in the merge set") {
        val set1 = from(Seq(1))
        val set2 = from(Seq(2))
        set1.merge(set2).toSet should ===(Set(1, 2))
      }

      it("case two: same elements will not be duplicated") {
        val set1 = from(Seq(1))
        val set2 = from(Seq(1))
        set1.merge(set2).toSet should ===(Set(1))
      }

      it("case three: if element in set1 is deleted in the other set, the element will not shown") {
        val uniqueTimestampClock = new UniqueTimestampClock()
        val set1 = from(Seq(1), uniqueTimestampClock)
        val set2 = emptySet[Int](uniqueTimestampClock).add(1).remove(1)
        set1.merge(set2).toSet should ===(Set())
      }

      it("case four: if deleted element in set1 is added back in the other set, the element will be shown") {
        val uniqueTimestampClock = new UniqueTimestampClock()
        val set1 = from(Seq(1), uniqueTimestampClock).remove(1)
        val set2 = from(Seq(1), uniqueTimestampClock)
        set1.merge(set2).toSet should ===(Set(1))
      }

      it("case five: if the latest add transaction has the same timestamp as the latest remove transaction, the element is treated as removed") {
        val fixedTimestampClock: LWWElementSetClock = () => Instant.ofEpochMilli(10000)
        val set = emptySet[Int](fixedTimestampClock)
        set.add(1).remove(1).toSet should ===(Set())
      }
    }

    describe("sanity checks") {
      it("should try to prevent null values") {
        shouldBeIllegal {
          LWWElementSet2(null, TimestampGSet[String]())(new LWWElementSetClockImpl)
        }

        shouldBeIllegal {
          LWWElementSet2(TimestampGSet[String](), null)(new LWWElementSetClockImpl)
        }

        shouldBeIllegal {
          LWWElementSet2(TimestampGSet[String](), TimestampGSet[String]())(null)
        }

        shouldBeIllegal {
          emptySet[String]().add(null)
        }

        shouldBeIllegal {
          emptySet[String]().remove(null)
        }

        shouldBeIllegal {
          emptySet[String]().compare(null)
        }

        shouldBeIllegal {
          emptySet[String]().merge(null)
        }
      }
    }
  }
}
