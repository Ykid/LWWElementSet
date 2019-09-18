package com.challenge

import org.scalatest.{FunSpec, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class PropertySpec extends FunSpec with Matchers with TimeMeasurementHelper with ScalaCheckPropertyChecks {
  describe("foo") {
    describe("ha") {
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

      it("commutativity") {
        forAll { (l1: List[(Boolean, Int)], l2: List[(Boolean, Int)]) =>
          val s1 = createSetFromList(l1)
          val s2 = createSetFromList(l2)
          s1.merge(s2) should ===(s2.merge(s1))
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
