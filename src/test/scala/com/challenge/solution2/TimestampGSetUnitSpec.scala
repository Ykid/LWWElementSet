package com.challenge.solution2

import java.time.Instant

import org.scalatest.{FunSpec, Matchers}

class TimestampGSetUnitSpec extends FunSpec with Matchers with TestUtil {

  describe("sanity checks") {
    it("should try to prevent null values") {
      shouldBeIllegal {
        TimestampGSet(null)
      }

      shouldBeIllegal {
        TimestampGSet[String]().add(null, Instant.ofEpochMilli(1L))
      }

      shouldBeIllegal {
        TimestampGSet[String]().add("foo", null)
      }

      shouldBeIllegal {
        TimestampGSet[String]().merge(null)
      }

      shouldBeIllegal {
        TimestampGSet[String]().compare(null)
      }
    }
  }
}
