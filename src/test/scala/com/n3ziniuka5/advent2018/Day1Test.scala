package com.n3ziniuka5.advent2018

import org.scalatest.FunSpec

class Day1Test extends FunSpec {
  describe("part1") {
    it("+1, +1, +1") {
      val input = List("+1", "+1", "+1")
      assert(Day1.solvePart1(input) == 3)
    }

    it("+1, +1, -2") {
      val input = List("+1", "+1", "-2")
      assert(Day1.solvePart1(input) == 0)
    }

    it("-1, -2, -3") {
      val input = List("-1", "-2", "-3")
      assert(Day1.solvePart1(input) == -6)
    }
  }

  describe("part2") {
    it("+1, -1") {
      val input = List("+1", "-1")
      assert(Day1.solvePart2(input) == 0)
    }

    it("+3, +3, +4, -2, -4") {
      val input = List("+3", "+3", "+4", "-2", "-4")
      assert(Day1.solvePart2(input) == 10)
    }

    it("-6, +3, +8, +5, -6") {
      val input = List("-6", "+3", "+8", "+5", "-6")
      assert(Day1.solvePart2(input) == 5)
    }

    it("+7, +7, -2, -7, -4") {
      val input = List("+7", "+7", "-2", "-7", "-4")
      assert(Day1.solvePart2(input) == 14)
    }
  }
}
