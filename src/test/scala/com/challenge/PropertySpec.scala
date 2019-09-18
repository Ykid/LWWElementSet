package com.challenge

import org.scalatest.{FunSpec, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class PropertySpec extends FunSpec with Matchers with TimeMeasurementHelper with ScalaCheckPropertyChecks {
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
          s1.merge(s1) should === (s1)
        }
      }

      it("commutativity")  {
        forAll { (l1: List[(Boolean, Int)], l2: List[(Boolean, Int)]) =>
          val s1 = createSetFromList(l1)
          val s2 = createSetFromList(l2)
          s1.merge(s2) should ===(s2.merge(s1))
        }
      }
    }

    describe("add and remove"){
      it("agree with a set if add and remove the same element randomly") {
        val ele = 1
        forAll{operations: List[Boolean] =>
          operations.foldRight((Set[Int](), LWWElementSet2()())) {
            case (isAdd, (referenceImpl, myImpl)) => {
              if (isAdd) {
                (referenceImpl + ele, myImpl.add(ele))
              } else {
                (referenceImpl - ele, myImpl.remove(ele))
              }
            }
          }
        }
      }

      it("agree with a set if add and remove (possibly) different element randomly") {
        forAll{operations: List[(Boolean, Int)] =>
          operations.foldRight((Set[Int](), LWWElementSet2()())) {
            case ((isAdd, ele), (referenceImpl, myImpl)) => {
              if (isAdd) {
                (referenceImpl + ele, myImpl.add(ele))
              } else {
                (referenceImpl - ele, myImpl.remove(ele))
              }
            }
          }
        }
      }
    }
  }

  def createSetFromList(l: List[(Boolean, Int)]): LWWElementSet2 = {
    l.foldLeft(LWWElementSet2()()) {
      case (set, (shouldAdd, ele))=> if (shouldAdd) set.add(ele) else set.remove(ele)
    }
  }
}
