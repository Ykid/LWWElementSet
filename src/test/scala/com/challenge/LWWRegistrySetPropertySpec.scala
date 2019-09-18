package com.challenge

import java.time.Instant

import com.challenge.LWWElementSet2.LWWRegistrySet
import org.scalatest.{FunSpec, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.collection.immutable.HashMap

class LWWRegistrySetPropertySpec extends FunSpec with Matchers with ScalaCheckPropertyChecks {

  def intToTs(i: Int) = {
    Instant.ofEpochMilli((i + 1).toLong)
  }

  describe("LWWRegistrySet") {
    describe("should satisfy partial order requirement") {
      it("should be reflexive") {
        forAll { elems: List[Int] =>
          val set = elems.zipWithIndex.foldLeft(LWWRegistrySet()) {
            case (accumulate, (elem, index)) => accumulate.add(elem, intToTs(index + 1))
          }
          set.compare(set) should be(true)
        }
      }

      it("should be anti-symmetric") {
        //TODO: maybe write a better generator ?
        forAll { elems: List[Int] =>
          val allStates = elems.zipWithIndex.scanLeft(LWWRegistrySet()) {
            case (accumulated, (elem, i)) => accumulated.add(elem, intToTs(i))
          }
          val zipped = allStates.zipWithIndex
          for {
            (state1, idx1) <- zipped
            (state2, idx2) <- zipped if idx1 <= idx2
          } {
            state1.compare(state2) shouldBe (true)
            if (state2.compare(state1)) {
              state1 should === (state2)
            }
          }
        }
      }

      it("should be transitive") {
        forAll { elems: List[Int] =>
          val allStates = elems.zipWithIndex.scanLeft(LWWRegistrySet()) {
            case (accumulated, (elem, i)) => accumulated.add(elem, intToTs(i))
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
  }

}
