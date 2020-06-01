package com.swissborg.firework

import cats.effect.IO
import cats.effect.concurrent.Deferred
import com.typesafe.scalalogging.StrictLogging

trait Firework {
  def boom: IO[Unit]
}

object Firework {
  final class Live(detonator: Deferred[IO, Unit]) extends Firework with StrictLogging {
    override val boom: IO[Unit] = {
      for {
        _ <- detonator.get
        _ <- IO(logger.info("BOOM!"))
      } yield ()
    }
  }
}
