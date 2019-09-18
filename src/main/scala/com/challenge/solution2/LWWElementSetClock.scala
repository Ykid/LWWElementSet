package com.challenge.solution2

import java.time.{Clock, Instant}

trait LWWElementSetClock {
  def now(): Instant
}

class LWWElementSetClockImpl extends LWWElementSetClock {
  private val clock: Clock = Clock.systemUTC()

  override def now(): Instant = clock.instant()
}

