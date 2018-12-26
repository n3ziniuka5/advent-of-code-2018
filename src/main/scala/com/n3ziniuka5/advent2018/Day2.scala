package com.n3ziniuka5.advent2018

import scala.annotation.tailrec

object Day2 {
  def main(args: Array[String]): Unit = {
    val input = Util.linesFromFile("day2.txt")

    println(s"Part1 Answer - ${solvePart1(input)}")
    println(s"Part2 Answer - ${solvePart2(input)}")
  }

  def charCountMap(ids: String): Map[Char, Int] = {
    @tailrec
    def loop(accum: Map[Char, Int], current: Char, remaining: List[Char]): Map[Char, Int] = {
      val newAccum = if(accum.contains(current)) {
        accum
      } else {
        val count = remaining.count(_ == current) + 1
        accum + (current -> count)
      }

      remaining match {
        case head :: tail => loop(newAccum, head, tail)
        case Nil => accum
      }
    }

    val charList = ids.toCharArray.toList
    loop(Map.empty, charList.head, charList.tail)
  }

  def containsChars(countMap: Map[Char, Int], numChars: Int): Boolean = {
    countMap.exists(_._2 == numChars)
  }

  def removeCharAt(str: String, pos: Int): String = {
    val (part1, part2) = str.splitAt(pos)
    part1 + part2.tail
  }

  def commonString(str1: String, str2: String): Option[String] = {
    @tailrec
    def loop(currentPos: Int, letterCount: Int, str1: String, str2: String): Option[String] = {
      val maybeCommonStr1 = removeCharAt(str1, currentPos)
      val maybeCommonStr2 = removeCharAt(str2, currentPos)

      if(maybeCommonStr1 == maybeCommonStr2) {
        Some(maybeCommonStr1)
      } else {
        if(currentPos == letterCount -1) {
          None
        } else {
          loop(currentPos + 1, letterCount, str1, str2)
        }
      }
    }

    val letterCount = str1.length
    loop(0, letterCount, str1, str2)
  }

  def solvePart1(ids: List[String]): Int = {
    val charCounts = ids.map(charCountMap)
    val contains2 = charCounts.count(containsChars(_, 2))
    val contains3 = charCounts.count(containsChars(_, 3))

    contains2 * contains3
  }

  def solvePart2(ids: List[String]): Option[String] = {
    def loop(currentString: String, remaining: List[String]): Option[String] = {
      val commonStringOpt = remaining
        .toStream
        .map(commonString(currentString, _))
        .collectFirst {
          case a if a.isDefined => a.get
        }

      commonStringOpt.fold {
        remaining match {
          case head :: tail => loop(head, tail)
          case Nil => None
        }
      }(Some(_))
    }

    loop(ids.head, ids.tail)
  }
}
