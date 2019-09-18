package com.challenge

import java.time.Instant

import com.challenge.LWWElementSet2.TimestampGSet
import org.scalatest.{FunSpec, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class TimestampGSetPropertySpec extends FunSpec with Matchers with ScalaCheckPropertyChecks {

  private def toTimestamp(i: Int) = {
    Instant.ofEpochMilli(i.toLong)
  }

  describe("LWWRegistrySet") {
    describe("should satisfy partial order requirement") {
      it("should be reflexive") {
        forAll { elems: List[Int] =>
          val set = elems.zipWithIndex.foldLeft(TimestampGSet[Int]()) {
            case (accumulate, (elem, index)) => accumulate.add(elem, toTimestamp(index))
          }
          set.compare(set) should be(true)
        }
      }

      it("should be anti-symmetric") {
        //TODO: maybe write a better generator ?
        forAll { elems: List[Int] =>
          val allStates = elems.zipWithIndex.scanLeft(TimestampGSet[Int]()) {
            case (accumulated, (elem, i)) => accumulated.add(elem, toTimestamp(i))
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
        forAll { elems: List[Int] =>
          val allStates = elems.zipWithIndex.scanLeft(TimestampGSet[Int]()) {
            case (accumulated, (elem, i)) => accumulated.add(elem, toTimestamp(i))
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

    //TODO: actually this is already covered in the above test cases
    describe("update operation") {
      it("should be monotonic") {
        forAll { elems: List[Int] =>
          val allStates = elems.zipWithIndex.scanLeft(TimestampGSet[Int]()) {
            case (accumulated, (elem, i)) => accumulated.add(elem, toTimestamp(i))
          }
          val zipped = allStates.zipWithIndex
          for {
            (state1, idx1) <- zipped if idx1 < (zipped.length - 1)
          } {
            val (state2, _) = zipped(idx1 + 1)
            state1.compare(state2) shouldBe (true)
            state2.compare(state1) shouldBe (false)
          }
        }
      }
    }
  }

}
