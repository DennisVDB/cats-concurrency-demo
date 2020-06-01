package com.swissborg.counter

import cats.effect.IO
import cats.effect.concurrent.Ref

trait Counter {
  def badIncrement: IO[Int]
  def increment: IO[Int]
}

object Counter {
  final class Live(counter: Ref[IO, Int]) extends Counter {
    override def badIncrement: IO[Int] = {
      for {
        v <- counter.get
        _ <- counter.set(v + 1)
      } yield v + 1
    }

    override def increment: IO[Int] = {
      counter.modify(v => (v + 1, v + 1))
    }
  }

  def make: IO[Counter] = Ref.of[IO, Int](0).map(counter => new Live(counter))
}
