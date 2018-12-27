package com.n3ziniuka5.advent2018

import org.scalatest.FunSpec
import Day3.Coords
import Day3.Claim

class Day3Test extends FunSpec {
  describe("parse input") {
    it("#1 @ 1,3: 4x4") {
      val expected = Claim(1, Coords(1, 3), 4, 4)
      assert(Day3.parseInputLine("#1 @ 1,3: 4x4") == expected)
    }

    it("#1341 @ 124,437: 17x23") {
      val expected = Claim(1341, Coords(124, 437), 17, 23)
      assert(Day3.parseInputLine("#1341 @ 124,437: 17x23") == expected)
    }
  }


  val testInput = List(
    "#1 @ 1,3: 4x4",
    "#2 @ 3,1: 4x4",
    "#3 @ 5,5: 2x2",
  )
  it("part1") {
    val expected = 4
    assert(Day3.solvePart1(testInput) == expected)
  }

  it("part2") {
    val expected = 3
    assert(Day3.solvePart2(testInput) == expected)
  }
}
