package com.challenge

import java.time.Instant

import org.scalatest.{FunSpec, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class PropertySpec extends FunSpec with Matchers with TimeMeasurementHelper with ScalaCheckPropertyChecks {

  //not thread safe, only appropriate for single thread testing
  class UniqueTimestampClock extends LWWElementSetClock {
    var counter: Long = 1

    override def now(): Instant = {
      val result = Instant.ofEpochMilli(counter)
      counter = counter + 1
      result
    }
  }

  describe("foo") {
    describe("merge") {
      it("associativity") {
        forAll { (l1: List[(Boolean, Int)], l2: List[(Boolean, Int)], l3: List[(Boolean, Int)]) =>
          val s1 = createSetFromList(l1)
          val s2 = createSetFromList(l2)
          val s3 = createSetFromList(l3)
          s1.merge(s2).merge(s3) should ===(s1.merge(s2.merge(s3)))
        }
      }

      it("idempotence") {
        forAll { l1: List[(Boolean, Int)] =>
          val s1 = createSetFromList(l1)
          s1.merge(s1) should ===(s1)
        }
      }

      it("commutativity") {
        forAll { (l1: List[(Boolean, Int)], l2: List[(Boolean, Int)]) =>
          val s1 = createSetFromList(l1)
          val s2 = createSetFromList(l2)
          s1.merge(s2) should ===(s2.merge(s1))
        }
      }
    }

    describe("add and remove") {
      it("agree with a set if add and remove the same element randomly") {
        val ele = 1
        forAll { operations: List[Boolean] =>
          val (reference, mine) = operations.foldRight((Set[Int](), LWWElementSet2()(new UniqueTimestampClock()))) {
            case (isAdd, (referenceImpl, myImpl)) => {
              if (isAdd) {
                (referenceImpl + ele, myImpl.add(ele))
              } else {
                (referenceImpl - ele, myImpl.remove(ele))
              }
            }
          }
          reference should ===(mine.toSet)
        }
      }

      it("agree with a set if add and remove (possibly) different element randomly") {
        forAll { operations: List[(Boolean, Int)] =>
          val (reference, mine) = operations.foldRight((Set[Int](), LWWElementSet2()(new UniqueTimestampClock()))) {
            case ((isAdd, ele), (referenceImpl, myImpl)) => {
              if (isAdd) {
                (referenceImpl + ele, myImpl.add(ele))
              } else {
                (referenceImpl - ele, myImpl.remove(ele))
              }
            }
          }
          reference should ===(mine.toSet)
        }
      }
    }
  }

  def createSetFromList(l: List[(Boolean, Int)]): LWWElementSet2 = {
    l.foldLeft(LWWElementSet2()()) {
      case (set, (shouldAdd, ele)) => if (shouldAdd) set.add(ele) else set.remove(ele)
    }
  }
}
