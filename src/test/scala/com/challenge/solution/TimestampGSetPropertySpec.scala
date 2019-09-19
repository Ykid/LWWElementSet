package com.challenge.solution

import org.scalatest.{FunSpec, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class TimestampGSetPropertySpec extends FunSpec with Matchers with ScalaCheckPropertyChecks with TestUtil {


  describe("TimestampGSet") {
    describe("should satisfy CvRDT requirement") {
      describe("should satisfy partial order requirement") {
        it("should be reflexive") {
          forAll { elems: List[Int] =>
            val set = createSetFromList(elems)
            set.compare(set) should be(true)
          }
        }

        it("should be anti-symmetric") {
          forAll { (l1: List[Int], l2: List[Int]) =>
            val s1 = createSetFromList(l1)
            val s2 = createSetFromList(l2)
            val s12 = s1.merge(s2)

            s1.compare(s12) should be(true)
            s2.compare(s12) should be(true)

            if (s12.compare(s1)) {
              s12 should ===(s1)
            }

            if (s12.compare(s2)) {
              s12 should ===(s2)
            }
          }
        }

        it("should be transitive") {
          forAll { (l1: List[Int], l2: List[Int], l3: List[Int]) =>
            val s1 = createSetFromList(l1)
            val s2 = createSetFromList(l2)
            val s3 = createSetFromList(l3)

            val s12 = s1.merge(s2)
            val s123 = s1.merge(s2).merge(s3)

            s1.compare(s12) should be(true)
            s2.compare(s12) should be(true)

            s12.compare(s123) should be(true)

            s1.compare(s123) should be(true)
            s2.compare(s123) should be(true)
          }
        }
      }

      describe("should 'monotonically increase' in state") {
        it("should be monotonic") {
          forAll { elems: List[Int] =>
            val allStates = elems.zipWithIndex.scanLeft(TimestampGSet[Int]()) {
              case (accumulated, (elem, i)) => accumulated.add(elem, toInstant(i))
            }
            val zipped = allStates.zipWithIndex
            for {
              (state1, idx1) <- zipped
              (state2, idx2) <- zipped if idx1 < idx2
            } {
              state1.compare(state2) shouldBe (true)
              state2.compare(state1) shouldBe (false)
            }
          }
        }
      }

      describe("the merge function has 'least upper bound' property and it is idempotent and order-independent") {
        it("associativity") {
          forAll { (l1: List[Int], l2: List[Int], l3: List[Int]) =>
            val s1 = createSetFromList(l1)
            val s2 = createSetFromList(l2)
            val s3 = createSetFromList(l3)
            s1.merge(s2).merge(s3) should ===(s1.merge(s2.merge(s3)))
          }
        }

        it("idempotence") {
          forAll { l1: List[Int] =>
            val s1 = createSetFromList(l1)
            s1.merge(s1) should ===(s1)
          }
        }

        it("commutativity") {
          forAll { (l1: List[Int], l2: List[Int]) =>
            val s1 = createSetFromList(l1)
            val s2 = createSetFromList(l2)
            s1.merge(s2) should ===(s2.merge(s1))
          }
        }

        it("the least upper bound operation requirement") {
          forAll { (l1: List[Int], l2: List[Int]) =>
            val s1 = createSetFromList(l1)
            val s2 = createSetFromList(l2)
            val merged = s1.merge(s2)
            s1.compare(merged) should be(true)
            s2.compare(merged) should be(true)
          }
        }
      }
    }
  }

  describe("its add operation") {
    it("should agree with a set for non null elements") {
      forAll { operations: List[Int] =>
        val (reference, mine) = operations.zipWithIndex.foldRight((Set[Int](), TimestampGSet[Int]())) {
          case ((ele, idx), (referenceImpl, myImpl)) =>
            (referenceImpl + ele, myImpl.add(ele, toInstant(idx)))
        }
        reference should ===(mine.toSet)
      }
    }
  }

  describe("compare operation") {
    it("should agree with merge operation") {
      forAll { (l1: List[Int], l2: List[Int]) =>
        val s1 = createSetFromList(l1)
        val s2 = createSetFromList(l2)
        val s3 = s1.merge(s2)

        s1.merge(s3) should ===(s3)
        s2.merge(s3) should ===(s3)

        s1.compare(s3) should be(true)
        s2.compare(s3) should be(true)
      }
    }
  }

  describe("serialization and deserialization") {
    it("use integer as element type: should serialize to protobuf and deserialize back to the same object") {
      import com.challenge.solution.serialization.IntConverter.defaultCoverter
      forAll { l1: List[Int] =>
        val s1: TimestampGSet[Int] = createSetFromList(l1)
        val serialized = TimestampGSet.serialize(s1)
        val deserialized: TimestampGSet[Int] = TimestampGSet.deserialize[Int](serialized).get

        deserialized should ===(s1)
      }
    }
  }

  private def createSetFromList[T](l: List[T]): TimestampGSet[T] = {
    l.zipWithIndex.foldLeft(TimestampGSet[T]()) {
      case (accumulate, (elem, index)) => accumulate.add(elem, toInstant(index))
    }
  }

}
