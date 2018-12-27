package com.n3ziniuka5.advent2018

object Day5 {
  def main(args: Array[String]): Unit = {
    val input = Util.linesFromFile("day5.txt").head

    println(s"Part1 Answer - ${solvePart1(input)}")
    println(s"Part2 Answer - ${solvePart2(input)}")
  }

  def charactersReact(a: Char, b: Char): Boolean = {
    a.toLower == b.toLower && a != b
  }

  def performReaction(polymer: String): String = {
    polymer.foldLeft("") { case (result, headChar) =>
      if(result.isEmpty) {
        headChar.toString
      } else {
        val lastChar = result.last
        if(charactersReact(headChar, lastChar)) {
          result.init
        } else {
          result + headChar
        }
      }
    }
  }

  def solvePart1(input: String): Int = {
    performReaction(input).length
  }

  def solvePart2(input: String): Int = {
    val distinctUnits = input.toLowerCase.distinct
    distinctUnits.toCharArray
      .map { unit =>
        val withoutUnit = input.filter(_.toLower != unit)
        performReaction(withoutUnit).length
      }
      .min
  }
}
