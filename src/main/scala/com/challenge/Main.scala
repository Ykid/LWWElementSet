package com.challenge

import java.time.{Clock, Instant, ZoneId}

object Main extends App {
  val lwwSet = new LWWElementSet()
  val set1 = lwwSet.add(1)
  val set2 = lwwSet.add(1).remove(1)
  Thread.sleep(5)
  val temp1 = set1.merge(set2)
  println(temp1)
  println(temp1.result())
  Thread.sleep(5)
  val temp2 = set1.merge(set2.remove(1).remove(1).remove(1))
  println(temp2)
  println(temp2.result())
}




