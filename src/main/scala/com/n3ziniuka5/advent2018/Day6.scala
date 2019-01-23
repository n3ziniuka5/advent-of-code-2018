package com.n3ziniuka5.advent2018

object Day6 {
  case class LocationCandidate(coords: Coords)
  case class Coords(x: Int, y: Int)
  case class Boundary(minX: Int, maxX: Int, minY: Int, maxY: Int, coords: List[Coords])

  def main(args: Array[String]): Unit = {
    val input = Util.linesFromFile("day6.txt")

    println(s"Part1 Answer - ${solvePart1(input)}")
    println(s"Part2 Answer - ${solvePart2(input, 10000)}")
  }

  def parse(input: List[String]): List[LocationCandidate] = {
    input.map(l => LocationCandidate(parseLine(l)))
  }

  def parseLine(inputLine: String): Coords = {
    def toInt(str: String): Int = str.trim.toInt

    val splitString = inputLine.split(",")
    Coords(toInt(splitString(0)), toInt(splitString(1)))
  }

  def manhattanDistance(a: Coords, b: Coords): Int = {
    math.abs(a.x - b.x) + math.abs(a.y - b.y)
  }

  def computeBoundary(locations: List[LocationCandidate]): Boundary = {
    val sortedX = locations.sortBy(_.coords.x)
    val sortedY = locations.sortBy(_.coords.y)

    val minX = sortedX.head.coords.x
    val maxX = sortedX.last.coords.x
    val minY = sortedY.head.coords.y
    val maxY = sortedY.last.coords.y

    val boundaryTop = (minX to maxX).map(Coords(_, minY))
    val boundaryBottom = (minX to maxX).map(Coords(_, maxY))
    val boundaryLeft = (minY to maxY).map(Coords(minX, _))
    val boundaryRight = (minY to maxY).map(Coords(maxX, _))
    val boundaryCoords = (boundaryTop ++ boundaryBottom ++ boundaryLeft ++ boundaryRight).toList

    Boundary(minX, maxX, minY, maxY, boundaryCoords)
  }

  def shortestDistanceMap(locations: List[LocationCandidate], coords: List[Coords]): Map[Coords, LocationCandidate] = {
    coords.flatMap { c =>
      val sortedLocationsWithDistance = locations.map(l => l -> manhattanDistance(c, l.coords)).sortBy(_._2)
      val sameDistanceToMultipleLocations = sortedLocationsWithDistance
        .take(2)
        .map { case (_, distance) =>
          distance
        }.distinct.size == 1

      if(sameDistanceToMultipleLocations) {
        None
      } else {
        Some(c -> sortedLocationsWithDistance.head._1)
      }
    }.toMap
  }

  def areaMap(distanceMap: Map[Coords, LocationCandidate]): Map[LocationCandidate, Int] = {
    distanceMap.groupBy(_._2).mapValues(_.size)
  }

  def fillBoundary(boundary: Boundary): List[Coords] = {
    val minX = boundary.minX + 1
    val maxX = boundary.maxX - 1
    val minY = boundary.minY + 1
    val maxY = boundary.maxY - 1

    val coords = for {
      x <- minX to maxX
      y <- minY to maxY
    } yield Coords(x, y)
    coords.toList
  }

  def solvePart1(input: List[String]): Int = {
    val locationCandidates = parse(input)
    val boundary = computeBoundary(locationCandidates)
    val filledBoundary = fillBoundary(boundary)

    val candidateAreas = areaMap(shortestDistanceMap(locationCandidates, filledBoundary))
    val boundaryDistanceMap = areaMap(shortestDistanceMap(locationCandidates, boundary.coords))

    val infiniteAreasFilteredOut = candidateAreas
      .filterKeys(!boundaryDistanceMap.contains(_))

    infiniteAreasFilteredOut.values.max
  }

  def sumOfDistances(coords: Coords, locations: List[LocationCandidate]): Int = {
    locations.map(l => manhattanDistance(coords, l.coords)).sum
  }

  def solvePart2(input: List[String], maxDistanceNotInclusive: Int): Int = {
    val locationCandidates = parse(input)
    val boundary = computeBoundary(locationCandidates)
    val filledBoundary = fillBoundary(boundary)

    filledBoundary
      .map(c => sumOfDistances(c, locationCandidates))
      .count(_ < maxDistanceNotInclusive)
  }
}
