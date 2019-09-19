package com.challenge.solution

import com.challenge.solution.LWWElementSet.{empty => emptySet}
import org.scalatest.{FunSpec, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class LWWElementSetPropertySpec extends FunSpec with Matchers with ScalaCheckPropertyChecks {

  describe("LWWElementSet") {
    describe("should satisfy CvRDT requirement") {
      describe("should satisfy partial order requirement") {
        it("should be reflexive") {
          forAll { l1: List[(Boolean, Int)] =>
            val s1 = createSetFromList(l1)
            s1.compare(s1) should be(true)
          }
        }

        it("should be anti-symmetric") {
          forAll { (l1: List[(Boolean, Int)], l2: List[(Boolean, Int)]) =>
            val s1 = createUniqueTsSetFromList(l1)
            val s2 = createUniqueTsSetFromList(l2)
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
          forAll { (l1: List[(Boolean, Int)], l2: List[(Boolean, Int)], l3: List[(Boolean, Int)]) =>
            val s1 = createUniqueTsSetFromList(l1)
            val s2 = createUniqueTsSetFromList(l2)
            val s3 = createUniqueTsSetFromList(l3)

            val s12 = s1.merge(s2)
            val s123 = s1.merge(s2).merge(s3)

            s1.compare(s12) should be(true)
            s2.compare(s12) should be(true)

            s12.compare(s123) should be (true)

            s1.compare(s123) should be (true)
            s2.compare(s123) should be (true)
          }
        }
      }

      describe("should 'monotonically increase' in state") {
        it("should be monotonic") {
          forAll { operations: List[(Boolean, Int)] =>
            operations.foldRight(emptySet[Int](new UniqueTimestampClock())) {
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

      describe("the merge function has 'least upper bound' property and it is idempotent and order-independent") {
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
    }

    //use unique timestamp clock to make sure time stamp agrees with causal order
    describe("its add and remove operations") {
      it("should agree with a set if add and remove the same element randomly") {
        val ele = 1
        forAll { operations: List[Boolean] =>
          val (reference, mine) = operations.foldRight((Set[Int](), emptySet[Int](new UniqueTimestampClock()))) {
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

      it("should agree with a set if add and remove (possibly) different element randomly") {
        forAll { operations: List[(Boolean, Int)] =>
          val (reference, mine) = operations.foldRight((Set[Int](), emptySet[Int](new UniqueTimestampClock()))) {
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

    describe("compare operation") {
      it("should agree with merge operation") {
        //For a join-semilattice, the order is induced by setting x ≤ y whenever x ∨ y = y.
        forAll { (l1: List[(Boolean, Int)], l2: List[(Boolean, Int)]) =>
          val s1 = createUniqueTsSetFromList(l1)
          val s2 = createUniqueTsSetFromList(l2)
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
        forAll { (l1: List[(Boolean, Int)]) =>
          val s1: LWWElementSet[Int] = createSetFromList(l1)
          val serialized = LWWElementSet.serialize(s1)
          val deserialized: LWWElementSet[Int] = LWWElementSet.deserialize[Int](serialized).get

          deserialized should ===(s1)
        }
      }
    }
  }



  private def createSetFromList(l: List[(Boolean, Int)]): LWWElementSet[Int] = {
    l.foldLeft(emptySet[Int]()) {
      case (set, (shouldAdd, ele)) => if (shouldAdd) set.add(ele) else set.remove(ele)
    }
  }

  private def createUniqueTsSetFromList(l: List[(Boolean, Int)]): LWWElementSet[Int] = {
    l.foldLeft(emptySet[Int](new UniqueTimestampClock())) {
      case (set, (shouldAdd, ele)) => if (shouldAdd) set.add(ele) else set.remove(ele)
    }
  }
}
