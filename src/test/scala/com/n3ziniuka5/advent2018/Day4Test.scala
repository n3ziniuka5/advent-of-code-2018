package com.n3ziniuka5.advent2018

import java.time.LocalDateTime

import com.n3ziniuka5.advent2018.Day4.{Event, FellAsleep, StartedShift, WokeUp}
import org.scalatest.FunSpec

class Day4Test extends FunSpec {
  val testInput = List(
    "[1518-11-02 00:50] wakes up",
    "[1518-11-01 00:25] wakes up",
    "[1518-11-01 00:00] Guard #10 begins shift",
    "[1518-11-03 00:05] Guard #10 begins shift",
    "[1518-11-01 00:30] falls asleep",
    "[1518-11-01 23:58] Guard #99 begins shift",
    "[1518-11-01 00:05] falls asleep",
    "[1518-11-04 00:02] Guard #99 begins shift",
    "[1518-11-01 00:55] wakes up",
    "[1518-11-02 00:40] falls asleep",
    "[1518-11-03 00:24] falls asleep",
    "[1518-11-03 00:29] wakes up",
    "[1518-11-05 00:45] falls asleep",
    "[1518-11-05 00:03] Guard #99 begins shift",
    "[1518-11-04 00:46] wakes up",
    "[1518-11-04 00:36] falls asleep",
    "[1518-11-05 00:55] wakes up",
  )

  it("part1") {
    val expected = 240
    assert(Day4.solvePart1(testInput) == expected)
  }

  it("part2") {
    val expected = 4455
    assert(Day4.solvePart2(testInput) == expected)
  }

  describe("parse input") {
    it("[1518-11-01 23:58] Guard #99 begins shift") {
      val expectedDate = LocalDateTime.of(1518, 11, 1, 23, 58)
      val expectedEvent = Event(expectedDate, StartedShift(99))
      assert(Day4.parseEvent("[1518-11-01 23:58] Guard #99 begins shift") == expectedEvent)
    }

    it("[1518-11-02 00:40] falls asleep") {
      val expectedDate = LocalDateTime.of(1518, 11, 2, 0, 40)
      val expectedEvent = Event(expectedDate, FellAsleep)
      assert(Day4.parseEvent("[1518-11-02 00:40] falls asleep") == expectedEvent)
    }

    it("[1518-11-02 00:50] wakes up") {
      val expectedDate = LocalDateTime.of(1518, 11, 2, 0, 50)
      val expectedEvent = Event(expectedDate, WokeUp)
      assert(Day4.parseEvent("[1518-11-02 00:50] wakes up") == expectedEvent)
    }

    it("sorts correctly") {
      val input = List(
        "[1518-11-02 00:50] wakes up",
        "[1518-11-03 00:24] falls asleep",
        "[1518-11-05 00:45] falls asleep",
        "[1518-11-02 00:40] falls asleep",
        "[1518-11-03 00:29] wakes up",
        "[1518-11-01 00:25] wakes up",
      )
      val expected = List(
        Event(LocalDateTime.of(1518, 11, 1, 0, 25), WokeUp),
        Event(LocalDateTime.of(1518, 11, 2, 0, 40), FellAsleep),
        Event(LocalDateTime.of(1518, 11, 2, 0, 50), WokeUp),
        Event(LocalDateTime.of(1518, 11, 3, 0, 24), FellAsleep),
        Event(LocalDateTime.of(1518, 11, 3, 0, 29), WokeUp),
        Event(LocalDateTime.of(1518, 11, 5, 0, 45), FellAsleep)
      )

      assert(Day4.parseAndSort(input) == expected)
    }
  }
}
