package com.n3ziniuka5.advent2018

import org.scalatest.FunSpec

class Day5Test extends FunSpec {
  val testInput = "dabAcCaCBAcCcaDA"

  describe("performs reaction") {
    it(testInput) {
      val expected = "dabCBAcaDA"
      assert(Day5.performReaction(testInput) == expected)
    }

    it("aAb") {
      val expected = "b"
      assert(Day5.performReaction("aAb") == expected)
    }

    it("baA") {
      val expected = "b"
      assert(Day5.performReaction("baA") == expected)
    }

    it("aA") {
      val expected = ""
      assert(Day5.performReaction("aA") == expected)
    }
  }

  it("part1") {
    val expected = 10
    assert(Day5.solvePart1(testInput) == expected)
  }

  it("part2") {
    val expected = 4
    assert(Day5.solvePart2(testInput) == expected)
  }
}
