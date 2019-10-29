package com.challenge.solution

import java.time.Instant

/*
 * not thread safe, only appropriate for single thread testing
 * mock a total order clock
 */
class UniqueTimestampClock() extends LWWElementSetClock {
  var counter: Long = 1

  override def now(): Instant = {
    val result = Instant.ofEpochMilli(counter)
    counter = counter + 1
    result
  }
}
