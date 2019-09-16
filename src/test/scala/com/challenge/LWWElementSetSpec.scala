package com.challenge

import org.scalatest.{FunSpec, Matchers}

class LWWElementSetSpec extends FunSpec with Matchers {

  /*
   * TODO: property base check ?
   */
  describe("A LWWElementSet") {
    describe("basic add functionality") {
      it ("should be able to add one element when empty") {
        val set = new LWWElementSet()
        set.add(1).result() should === (Set(1))
      }

      it ("should add duplicate element only once") {
        val set = new LWWElementSet()
        set.add(1).add(1).result() should === (Set(1))
      }
    }

    describe("basic remove functionality") {
      it("should be able remove element if the set contains it") {
        val set = new LWWElementSet()
        set.add(1).remove(1).result() should === (Set())
      }

      it("should do nothing it the element to be removed is not in the set") {
        val set = new LWWElementSet()
        set.add(1).remove(2).result() should === (Set(1))
      }
    }

    describe("merge functionality") {
    }
  }
}
