package com.n3ziniuka5.advent2018

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import Util.localDateTimeOrdering

import scala.annotation.tailrec

object Day4 {
  private val eventRegex = "^\\[(.*)\\] (.*)$".r
  private val shiftStartRegex = "^Guard #(\\d+) begins shift$".r
  private val eventDateFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")

  sealed trait EventType
  case class StartedShift(id: Int) extends EventType
  case object FellAsleep extends EventType
  case object WokeUp extends EventType

  case class Event(time: LocalDateTime, eventType: EventType)
  case class SleepInterval(guardId: Int, minutesAsleep: List[Int])

  def main(args: Array[String]): Unit = {
    val input = Util.linesFromFile("day4.txt")

    println(s"Part1 Answer - ${solvePart1(input)}")
    println(s"Part2 Answer - ${solvePart2(input)}")
  }

  def solvePart1(input: List[String]): Int = {
    val sleepMap = parseAndBuildSleepMap(input)

    val (guardId, sleepMinutes) = sleepMap.maxBy { case (_, minutesAsleep) => minutesAsleep.size }
    val pickMinute = Util.mostOftenOccurrence(sleepMinutes)

    guardId * pickMinute
  }

  def solvePart2(input: List[String]): Int = {
    val sleepMap = parseAndBuildSleepMap(input)

    val guardWithTheirSleepiestMinute = sleepMap.mapValues(Util.mostOftenOccurrenceWithCount)
    val (guardId, (pickMinute, _)) = guardWithTheirSleepiestMinute.maxBy { case (_, (_, count)) => count }

    guardId * pickMinute
  }

  def parseAndBuildSleepMap(input: List[String]): Map[Int, List[Int]] = {
    val parsed = parseAndSort(input)
    buildSleepMap(parsed)
  }

  def buildSleepMap(events: List[Event]): Map[Int, List[Int]] = {
    val sleepSchedule = buildSleepIntervals(events)
    sleepSchedule.groupBy(_.guardId).mapValues { schedules =>
      schedules.flatMap(_.minutesAsleep)
    }
  }

  def buildSleepIntervals(events: List[Event]): List[SleepInterval] = {
    case class LoopState(
      guardId: Int,
      minuteStartedSleeping: Option[Int]
    )

    @tailrec
    def loop(loopState: Option[LoopState],
             currentEvent: Event,
             remainingEvents: List[Event],
             result: List[SleepInterval]
            ): List[SleepInterval] = {
      currentEvent.eventType match {
        case StartedShift(guardId) =>
          loop(Some(LoopState(guardId, None)), remainingEvents.head, remainingEvents.tail, result)

        case FellAsleep =>
          val newState = loopState.map(_.copy(minuteStartedSleeping = Some(currentEvent.time.getMinute)))
          loop(newState, remainingEvents.head, remainingEvents.tail, result)

        case WokeUp =>
          val newState = loopState.map(_.copy(minuteStartedSleeping = None))
          val additionalResult = for {
            state <- loopState
            fellAsleepAt <- state.minuteStartedSleeping
          } yield {
            val minutesAsleep = (fellAsleepAt until currentEvent.time.getMinute).toList
            SleepInterval(state.guardId, minutesAsleep)
          }

          val additionalResultAsList = additionalResult.map(List(_)).getOrElse(List.empty)
          val newResults = additionalResultAsList ++ result

          remainingEvents match {
            case head :: tail => loop(newState, head, tail, newResults)
            case Nil => newResults
          }
      }
    }

    loop(None, events.head, events.tail, List.empty)
  }

  def parseAndSort(input: List[String]): List[Event] = {
    input.map(parseEvent).sortBy(_.time)
  }

  def parseEventType(str: String): EventType = {
    str match {
      case "falls asleep" => FellAsleep
      case "wakes up" => WokeUp
      case shiftStartRegex(guardId) => StartedShift(guardId.toInt)
    }
  }

  def parseEvent(str: String): Event = {
    val regexMatch = eventRegex.findFirstMatchIn(str).get
    val dateString = regexMatch.group(1)
    val eventTypeString = regexMatch.group(2)

    val date = LocalDateTime.parse(dateString, eventDateFormat)
    val eventType = parseEventType(eventTypeString)

    Event(date, eventType)
  }
}
