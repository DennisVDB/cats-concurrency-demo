package com.swissborg.firework

import cats.effect.concurrent.Deferred
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import com.typesafe.scalalogging.StrictLogging

import scala.concurrent.duration._

object FireworkMain extends IOApp with StrictLogging {
  override def run(args: List[String]): IO[ExitCode] = {
    for {
      detonator <- Deferred[IO, Unit]
      _         <- List.fill(16)(new Firework.Live(detonator).boom).parSequence_.start

      _ <- IO(logger.info("3..."))
      _ <- IO.sleep(1.seconds)
      _ <- IO(logger.info("2..."))
      _ <- IO.sleep(1.seconds)
      _ <- IO(logger.info("1..."))
      _ <- IO.sleep(1.seconds)

      _ <- detonator.complete(())

      _ <- IO.never
    } yield ExitCode.Success
  }
}
