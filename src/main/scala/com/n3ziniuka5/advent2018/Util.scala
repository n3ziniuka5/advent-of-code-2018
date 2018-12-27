package com.n3ziniuka5.advent2018

import java.time.{LocalDateTime, ZoneOffset}

import scala.io.Source

object Util {
  def linesFromFile(fileName: String): List[String] = {
    Source.fromResource(fileName).getLines().toList
  }

  implicit val localDateTimeOrdering: Ordering[LocalDateTime] = Ordering.by(_.toEpochSecond(ZoneOffset.UTC))

  def occurrenceMap[A](list: List[A]): Map[A, Int] = {
    list.groupBy(identity).mapValues(_.length)
  }

  def mostOftenOccurrence[A](list: List[A]): A = {
    list.groupBy(identity).maxBy(_._2.size)._1
  }

  def mostOftenOccurrenceWithCount[A](list: List[A]): (A, Int) = {
    list.groupBy(identity).mapValues(_.size).maxBy { case (_, count) => count }
  }

  implicit class ListOps[A](val self: List[A]) extends AnyVal {
    def occurrenceMap: Map[A, Int] = Util.occurrenceMap(self)

    def mostOftenOccurrence: A = Util.mostOftenOccurrence(self)

    def mostOftenOccurrenceWithCount: (A, Int) = Util.mostOftenOccurrenceWithCount(self)
  }
}
