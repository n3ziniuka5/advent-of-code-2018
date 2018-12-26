package com.n3ziniuka5.advent2018

import scala.annotation.tailrec

object Day1 {
  def main(args: Array[String]): Unit = {
    val input = Util.linesFromFile("day1.txt")

    println(s"Part1 Answer - ${solvePart1(input)}")
    println(s"Part2 Answer - ${solvePart2(input)}")
  }

  def changeFrequency(current: Int, changeString: String): Int = {
    val trimmedInput = changeString.trim
    val operation = trimmedInput.head
    val number = trimmedInput.tail.toInt
    if(operation == '+') {
      current + number
    } else {
      current - number
    }
  }

  def solvePart2(input: List[String]): Int = {
    @tailrec
    def loop(current: Int,
             seen: Set[Int],
             allChanges: List[String],
             currentChange: String,
             remainingChanges: List[String]
            ): Int = {
      val newFrequency = changeFrequency(current, currentChange)
      if(seen.contains(newFrequency)) {
        newFrequency
      } else {
        val newSeen = seen + newFrequency
        remainingChanges match {
          case head :: tails => loop(newFrequency, newSeen, allChanges, head, tails)
          case Nil => loop(newFrequency, newSeen, allChanges, allChanges.head, allChanges.tail)
        }
      }
    }

    loop(0, Set(0), input, input.head, input.tail)
  }

  def solvePart1(input: List[String]): Int = {
    input.foldLeft(0) { case (acc, strNum) =>
      changeFrequency(acc, strNum)
    }
  }
}
