package com.n3ziniuka5.advent2018

import Util.ListOps

object Day3 {
  case class Coords(x: Int, y: Int) {
    def +(other: Coords): Coords = Coords(x + other.x, y + other.y)
  }

  case class Claim(id: Int, coords: Coords, width: Int, height: Int)

  private val inputRegex = "^#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)$".r

  def main(args: Array[String]): Unit = {
    val input = Util.linesFromFile("day3.txt")

    println(s"Part1 Answer - ${solvePart1(input)}")
    println(s"Part2 Answer - ${solvePart2(input)}")
  }

  def buildCoverageMap(input: List[Claim]): Map[Coords, Int] = {
    input
      .flatMap(c => areaOfCoords(c.coords, c.width, c.height))
      .occurrenceMap
  }

  def solvePart1(input: List[String]): Int = {
    val parsed = input.map(parseInputLine)
    val coverageMap = buildCoverageMap(parsed)
    coverageMap.count { case (_, count) => count > 1 }
  }

  def solvePart2(input: List[String]): Int = {
    val parsed = input.map(parseInputLine)
    val coverageMap = buildCoverageMap(parsed)

    val resultClaim = parsed.find { claim =>
      areaOfCoords(claim.coords, claim.width, claim.height).forall { coords =>
        coverageMap(coords) == 1
      }
    }
    resultClaim.get.id
  }

  def areaOfCoords(start: Coords, width: Int, height: Int): List[Coords] = {
    val range = for {
      x <- 1 to width
      y <- 1 to height
    } yield start + Coords(x, y)
    range.toList
  }

  def parseInputLine(str: String): Claim = {
    val regexMatch = inputRegex.findFirstMatchIn(str).get
    val id = regexMatch.group(1).toInt
    val x = regexMatch.group(2).toInt
    val y = regexMatch.group(3).toInt
    val width = regexMatch.group(4).toInt
    val height = regexMatch.group(5).toInt

    Claim(id, Coords(x, y), width, height)
  }
}
