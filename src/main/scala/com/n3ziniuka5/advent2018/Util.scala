package com.n3ziniuka5.advent2018

import scala.io.Source

object Util {
  def linesFromFile(fileName: String): List[String] = {
    Source.fromResource(fileName).getLines().toList
  }
}
