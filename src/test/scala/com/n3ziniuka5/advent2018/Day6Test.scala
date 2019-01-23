package com.n3ziniuka5.advent2018

import org.scalatest.{FunSpec, Matchers}
import Day6.Coords

class Day6Test extends FunSpec with Matchers {
  describe("parses input correctly") {
    it("1, 30") {
      val expected = Coords(1, 30)
      assert(Day6.parseLine("1, 30") == expected)
    }
  }

  describe("computes manhattan distance correctly") {
    it("1,1 - 1,2") {
      val expected = 1
      val a = Coords(1, 1)
      val b = Coords(1, 2)
      Day6.manhattanDistance(a, b) shouldEqual expected
    }

    it("1,1 - 2,2") {
      val expected = 2
      val a = Coords(1, 1)
      val b = Coords(2, 2)
      Day6.manhattanDistance(a, b) shouldEqual expected
    }

    it("1,1 - 3,3") {
      val expected = 4
      val a = Coords(1, 1)
      val b = Coords(3, 3)
      Day6.manhattanDistance(a, b) shouldEqual expected
    }

    it("1,1 - 5,1") {
      val expected = 4
      val a = Coords(1, 1)
      val b = Coords(5, 1)
      Day6.manhattanDistance(a, b) shouldEqual expected
    }
  }

  val testInput = List(
    "1, 1",
    "1, 6",
    "8, 3",
    "3, 4",
    "5, 5",
    "8, 9",
  )
  it("computes boundary correctly") {
    val expectedTop = (1 to 8).map(x => Coords(x, 1)).toList
    val expectedBottom = (1 to 8).map(x => Coords(x, 9)).toList
    val expectedLeft = (1 to 9).map(y => Coords(1, y)).toList
    val expectedRight = (1 to 9).map(y => Coords(8, y)).toList

    val expected = expectedTop ++ expectedBottom ++ expectedLeft ++ expectedRight

    val parsedInput = Day6.parse(testInput)
    Day6.computeBoundary(parsedInput).coords should contain theSameElementsAs expected
  }

  describe("part1") {
    it("example input") {
      val expected = 17
      assert(Day6.solvePart1(testInput) == expected)
    }

    it("infinite areas are discarded") {
      val input = List(
        "5, 5",
        "8, 8",
        "9, 9",
        "9, 6",
        "7, 9",
      )

      val expected = 2
      assert(Day6.solvePart1(input) == expected)
    }
  }

  describe("part2") {
    it("example input") {
      val expected = 16
      assert(Day6.solvePart2(testInput, 32) == expected)
    }
  }
}
