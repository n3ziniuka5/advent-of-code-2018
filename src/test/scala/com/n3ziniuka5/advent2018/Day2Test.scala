package com.n3ziniuka5.advent2018

import org.scalatest.FunSpec

class Day2Test extends FunSpec {
  it("part1") {
    val input = List(
      "abcdef",
      "bababc",
      "abbcde",
      "abcccd",
      "aabcdd",
      "abcdee",
      "ababab",
    )
    assert(Day2.solvePart1(input) == 12)
  }

  it("part2") {
    val input = List(
      "abcde",
      "fghij",
      "klmno",
      "pqrst",
      "fguij",
      "axcye",
      "wvxyz",
    )
    assert(Day2.solvePart2(input).contains("fgij"))
  }
}
