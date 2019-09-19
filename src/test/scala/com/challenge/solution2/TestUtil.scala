package com.challenge.solution2

import java.time.Instant

import org.scalatest.Matchers

trait TestUtil {
  this: Matchers =>
  def shouldBeIllegal(block: => Unit): Unit = {
    intercept[IllegalArgumentException](block)
  }

  def toInstant(i: Int): Instant = {
    Instant.ofEpochMilli(i.toLong)
  }
}
