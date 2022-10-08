package net.degoes.zio

import zio._

object Retry {

  //Schedule[-Env, -In, +Out]

  //Schedule A : |XXXXXXXX|            |XXXXXX|                |XXXXXXX|          |XXX|    |XX|
  //Schedule B :     |XXXX|    |XXXX|       |XXXXXX|               |XXX|         |XX|
  //A isect  B :     |XXXX|                 |X|                    |XXX|           |X|
  //A union  B : |XXXXXXXX|    |XXXX|  |XXXXXX|                |XXXXXXX|         |XXXX|    |XX|

  val policy = (Schedule.recurs(100) &&
    Schedule
      .exponential(10.millis)
      .whileOutput(_ < 5.second)
      .andThen(
        Schedule.fixed(30.second) && Schedule.recurs(20)
      )).jittered.tapInput((input: Exception) => ZIO.logError(input.toString()))

  Console.printLine("foo").retry(policy)

  /**
   * EXERCISE
   *
   * Using `Schedule.recurs`, create a schedule that recurs 5 times.
   */
  val fiveTimes = Schedule.recurs(5)

  /**
   * EXERCISE
   *
   * Using the `ZIO.repeat`, repeat printing "Hello World" five times to the
   * console.
   */
  val repeated1 = Console.printLine("Hello World").repeat(Schedule.recurs(5))

  /**
   * EXERCISE
   *
   * Using `Schedule.spaced`, create a schedule that recurs forever every 1 second.
   */
  val everySecond = Schedule.spaced(1.second)

  /**
   * EXERCISE
   *
   * Using the `&&` method of the `Schedule` object, the `fiveTimes` schedule,
   * and the `everySecond` schedule, create a schedule that repeats fives times,
   * evey second.
   */
  val fiveTimesEverySecond = Schedule.recurs(5) && Schedule.spaced(1.second)

  /**
   * EXERCISE
   *
   * Using the `ZIO#repeat`, repeat the action Console.printLine("Hi hi") using
   * `fiveTimesEverySecond`.
   */
  val repeated2 = Console.printLine("Hi hi").repeat(Schedule.recurs(5) && Schedule.spaced(1.second))

  /**
   * EXERCISE
   *
   * Using `Schedule#andThen` the `fiveTimes` schedule, and the `everySecond`
   * schedule, create a schedule that repeats fives times rapidly, and then
   * repeats every second forever.
   */
  val fiveTimesThenEverySecond = Schedule.recurs(5).andThen(Schedule.spaced(1.second))

  /**
   * EXERCISE
   *
   * Using `ZIO#retry`, retry the following error a total of five times.
   */
  val error1   = ZIO.fail("Uh oh!")
  val retried5 = error1.repeat(Schedule.recurs(5))

  /**
   * EXERCISE
   *
   * Using the `Schedule#||`, the `fiveTimes` schedule, and the `everySecond`
   * schedule, create a schedule that repeats the minimum of five times and
   * every second.
   */
  val fiveTimesOrEverySecond = Schedule.recurs(5) || Schedule.spaced(1.second)

  /**
   * EXERCISE
   *
   * Using `Schedule.exponential`, create an exponential schedule that starts
   * from 10 milliseconds.
   */
  val exponentialSchedule = Schedule.exponential(10.millis)

  /**
   * EXERCISE
   *
   * Using `Schedule.jittered` produced a jittered version of `exponentialSchedule`.
   */
  val jitteredExponential = ???

  /**
   * EXERCISE
   *
   * Using `Schedule.whileOutput`, produce a filtered schedule from `Schedule.forever`
   * that will halt when the number of recurrences exceeds 100.
   */
  val oneHundred = Schedule.forever.whileOutput(_ <= 100)

  /**
   * EXERCISE
   *
   * Using `Schedule.identity`, produce a schedule that recurs forever, without delay,
   * returning its inputs.
   */
  def inputs[A]: Schedule[Any, A, A] = ???

  /**
   * EXERCISE
   *
   * Using `Schedule#collect`, produce a schedule that recurs forever, collecting its
   * inputs into a list.
   */
  def collectedInputs[A]: Schedule[Any, A, List[A]] = ???

  /**
   * EXERCISE
   *
   * Using  `*>` (`zipRight`), combine `fiveTimes` and `everySecond` but return
   * the output of `everySecond`.
   */
  val fiveTimesEverySecondR = ???

  /**
   * EXERCISE
   *
   * Produce a jittered schedule that first does exponential spacing (starting
   * from 10 milliseconds), but then after the spacing reaches 60 seconds,
   * switches over to fixed spacing of 60 seconds between recurrences, but will
   * only do that for up to 100 times, and produce a list of the inputs to
   * the schedule.
   */
  import Schedule.{ collectAll, exponential, fixed, recurs }
  def mySchedule[A]: Schedule[Any, A, List[A]] = ???
}
