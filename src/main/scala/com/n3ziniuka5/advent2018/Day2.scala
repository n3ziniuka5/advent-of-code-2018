package com.n3ziniuka5.advent2018

import scala.annotation.tailrec

object Day2 {
  def main(args: Array[String]): Unit = {
    val input = Util.linesFromFile("day2.txt")

    println(s"Part1 Answer - ${solvePart1(input)}")
    println(s"Part2 Answer - ${solvePart2(input)}")
  }

  def charCountMap(ids: String): Map[Char, Int] = {
    ids.groupBy(identity).mapValues(_.length)
  }

  def containsCharsWithCertainCount(countMap: Map[Char, Int], count: Int): Boolean = {
    countMap.exists(_._2 == count)
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
    val charCountList = ids.map(charCountMap)
    val contains2 = charCountList.count(containsCharsWithCertainCount(_, 2))
    val contains3 = charCountList.count(containsCharsWithCertainCount(_, 3))

    contains2 * contains3
  }

  def solvePart2(ids: List[String]): Option[String] = {
    @tailrec
    def loop(currentString: String, remaining: List[String]): Option[String] = {
      val commonStringOpt = remaining
        .toStream
        .map(commonString(currentString, _))
        .collectFirst {
          case a if a.isDefined => a.get
        }

      remaining match {
        case head :: tail if commonStringOpt.isEmpty => loop(head, tail)
        case _ => commonStringOpt
      }
    }

    loop(ids.head, ids.tail)
  }
}
