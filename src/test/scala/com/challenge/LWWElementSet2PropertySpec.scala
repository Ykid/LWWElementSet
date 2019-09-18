package com.challenge

import java.time.Instant

import org.scalatest.{FunSpec, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class LWWElementSet2PropertySpec extends FunSpec with Matchers with TimeMeasurementHelper with ScalaCheckPropertyChecks {

  describe("LWWElementSet2") {
    describe("should satisfy partial order requirement") {
      it("should be reflexive") {
        forAll { l1: List[(Boolean, Int)] =>
          val s1 = createSetFromList(l1)
          s1.compare(s1) should be(true)
        }
      }

      //TODO: don't know how to generate perfect random data such that A <= B
      it("should be anti-symmetric") {
        //TODO: maybe write a better generator ?
        forAll { elems: List[(Boolean, Int)] =>
          val allStates = elems.scanLeft(LWWElementSet2[Int]()(new UniqueTimestampClock())) {
            case (accumulated, (isAdd, elem)) =>
              if (isAdd) accumulated.add(elem)
              else accumulated.remove(elem)
          }
          val zipped = allStates.zipWithIndex
          for {
            (state1, idx1) <- zipped
            (state2, idx2) <- zipped if idx1 <= idx2
          } {
            state1.compare(state2) shouldBe (true)
            if (state2.compare(state1)) {
              state1 should ===(state2)
            }
          }
        }
      }

      it("should be transitive") {
        forAll { elems: List[(Boolean, Int)] =>
          val allStates = elems.scanLeft(LWWElementSet2[Int]()(new UniqueTimestampClock())) {
            case (accumulated, (isAdd, elem)) =>
              if (isAdd) accumulated.add(elem)
              else accumulated.remove(elem)
          }
          val zipped = allStates.zipWithIndex
          for {
            (state1, idx1) <- zipped
            (state2, idx2) <- zipped if idx1 <= idx2
            (state3, idx3) <- zipped if idx2 <= idx3
          } {
            state1.compare(state2) shouldBe (true)
            state2.compare(state3) shouldBe (true)
            state1.compare(state3) shouldBe (true)
          }
        }
      }
    }

    describe("merge method should satisfy") {
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

      it("the least upper bound operation requirement") {
        forAll { (l1: List[(Boolean, Int)], l2: List[(Boolean, Int)]) =>
          val s1 = createSetFromList(l1)
          val s2 = createSetFromList(l2)
          val merged = s1.merge(s2)
          s1.compare(merged) should be(true)
          s2.compare(merged) should be(true)
        }
      }
    }

    //use unique timestamp clock to make sure time stamp agrees with causal order
    describe("add and remove") {
      it("agree with a set if add and remove the same element randomly") {
        val ele = 1
        forAll { operations: List[Boolean] =>
          val (reference, mine) = operations.foldRight((Set[Int](), LWWElementSet2[Int]()(new UniqueTimestampClock()))) {
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
          val (reference, mine) = operations.foldRight((Set[Int](), LWWElementSet2[Int]()(new UniqueTimestampClock()))) {
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

      it("should be monotonic") {
        forAll { operations: List[(Boolean, Int)] =>
          operations.foldRight(LWWElementSet2[Int]()(new UniqueTimestampClock())) {
            case ((isAdd, ele), last) =>
              val updated = if (isAdd) last.add(ele) else last.remove(ele)
              last.compare(updated) should be(true)
              //in case the element to be removed is not in the set
              if (last.lookup(ele)) {
                updated.compare(last) should be(false)
              }
              updated
          }
        }
      }
    }
  }

  private def createSetFromList(l: List[(Boolean, Int)]): LWWElementSet2[Int] = {
    l.foldLeft(LWWElementSet2[Int]()()) {
      case (set, (shouldAdd, ele)) => if (shouldAdd) set.add(ele) else set.remove(ele)
    }
  }

  //not thread safe, only appropriate for single thread testing
  class UniqueTimestampClock extends LWWElementSetClock {
    var counter: Long = 1

    override def now(): Instant = {
      val result = Instant.ofEpochMilli(counter)
      counter = counter + 1
      result
    }
  }

}
