package com.swissborg.counter

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import com.typesafe.scalalogging.StrictLogging

object CounterMain extends IOApp with StrictLogging {
  override def run(args: List[String]): IO[ExitCode] = {
    for {
      counter <- Counter.make
      r       <- List.fill(16)(counter.increment).parSequence
      _       <- IO(logger.info("{}", r.sorted))
    } yield ExitCode.Success
  }
}
