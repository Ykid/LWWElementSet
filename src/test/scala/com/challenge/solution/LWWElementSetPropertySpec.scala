package com.challenge.solution

import com.challenge.solution.LWWElementSet.{empty => emptySet}
import org.scalatest.{FunSpec, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class LWWElementSetPropertySpec extends FunSpec with Matchers with ScalaCheckPropertyChecks {

  describe("LWWElementSet") {
    /*
      Since it is based on LWW, this data type is convergent.
      https://github.com/pfrazee/crdt_notes#lww-element-set


      A state-based CRDT (CvRDT) must...
        * Have a partial order to the values.
        * "Monotonically increase" in state, meaning a new state only ever succeeds the current state in the value's ordering.
        * Define a merge function ("least upper bound") which is idempotent and order-independent.
    */
    describe("should satisfy CvRDT requirement") {
      /*
      * Partial order requirement
        * a ≤ a (reflexivity: every element is related to itself).
        * if a ≤ b and b ≤ a, then a = b (antisymmetry: two distinct elements cannot be related in both directions).
        * if a ≤ b and b ≤ c, then a ≤ c
        *   (transitivity: if a first element is related to a second element, and,
        *   in turn, that element is related to a third element, then the first element is related to the third element).
       */
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

            s12.compare(s123) should be(true)

            s1.compare(s123) should be(true)
            s2.compare(s123) should be(true)
          }
        }
      }

      /*
       * 1. "Monotonically increase" in state, meaning a new state only ever succeeds the current state in the value's ordering.
       * 2.
       * https://en.wikipedia.org/wiki/Conflict-free_replicated_data_type#State-based_CRDTs
       * The update function must monotonically increase the internal state, according to the same partial order rules as the semilattice.
       */
      describe("should 'monotonically increase' in state") {
        it("should be monotonic") {
          forAll { operations: List[(Boolean, Int)] =>
            operations.foldRight(emptySet[Int](new UniqueTimestampClock())) {
              case ((isAdd, ele), last) =>
                val updated = if (isAdd) last.add(ele) else last.remove(ele)
                last.compare(updated) should be(true)
                //in case the element to be removed is not in the set
                if (last.query(ele)) {
                  updated.compare(last) should be(false)
                }
                updated
            }
          }
        }
      }

      /*
       * 1. Define a merge function ("least upper bound") which is idempotent and order-independent.
       *
       * 2.
       * https://en.wikipedia.org/wiki/Conflict-free_replicated_data_type#State-based_CRDTs
       * CvRDTs send their full local state to other replicas, where the states are merged by a function which must be
       *  commutative,
       *  associative, and
       *  idempotent.
       *
       * The merge function provides a join for any pair of replica states, so the set of all states forms a semilattice
       *
       * The three properties above are actually definition of semilattice.
       */
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

        //it is upper bound, but may not be the least upper bound
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
      it("should agree with a set if add and remove the same element(non null) randomly") {
        val ele = 1
        forAll { operations: List[Boolean] =>
          val (reference, mine) = operations.foldRight((Set[Int](), emptySet[Int](new UniqueTimestampClock()))) {
            case (isAdd, (referenceImpl, myImpl)) =>
              if (isAdd) {
                (referenceImpl + ele, myImpl.add(ele))
              } else {
                (referenceImpl - ele, myImpl.remove(ele))
              }
          }
          reference should ===(mine.toSet)
        }
      }

      it("should agree with a set if add and remove (possibly) different element(non null) randomly") {
        forAll { operations: List[(Boolean, Int)] =>
          val (reference, mine) = operations.foldRight((Set[Int](), emptySet[Int](new UniqueTimestampClock()))) {
            case ((isAdd, ele), (referenceImpl, myImpl)) =>
              if (isAdd) {
                (referenceImpl + ele, myImpl.add(ele))
              } else {
                (referenceImpl - ele, myImpl.remove(ele))
              }
          }
          reference should ===(mine.toSet)
        }
      }
    }

    describe("query operation") {
      it("should be consistent with add and remove operation") {
        forAll { operations: List[(Boolean, Int)] =>
          operations.foldRight(emptySet[Int](new UniqueTimestampClock())) {
            case ((isAdd, ele), myImpl) =>
              if (isAdd) {
                val next = myImpl.add(ele)
                next.query(ele) should be(true)
                next
              } else {
                val next = myImpl.remove(ele)
                next.query(ele) should be(false)
                next
              }
          }
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
        import com.challenge.solution.serialization.IntConverter.defaultConverter
        forAll { l1: List[(Boolean, Int)] =>
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
