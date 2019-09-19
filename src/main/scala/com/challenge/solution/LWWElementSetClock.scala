package com.challenge.solution

import java.time.{Clock, Instant}

trait LWWElementSetClock {
  def now(): Instant
}

class LWWElementSetClockImpl extends LWWElementSetClock {
  private val clock: Clock = Clock.systemUTC()

  override def now(): Instant = clock.instant()
}
