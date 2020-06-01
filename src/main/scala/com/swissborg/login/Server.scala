package com.swissborg.login

import java.util.concurrent.TimeUnit.NANOSECONDS

import cats.effect.concurrent.{Deferred, Ref}
import cats.effect.{Clock, Concurrent, IO}
import cats.implicits._
import com.typesafe.scalalogging.StrictLogging

import scala.concurrent.duration.FiniteDuration
import scala.util.Random

trait Server {
  def request(token: String): IO[Option[Unit]]
  def login: IO[String]
}

object Server {
  final class Live(maxInactivity: FiniteDuration, state: Ref[IO, State])(implicit C: Concurrent[IO], clock: Clock[IO])
      extends Server
      with StrictLogging {
    override def request(token: String): IO[Option[Unit]] = {
      for {
        ts <- clock.monotonic(NANOSECONDS)
        r <- state.modify {
              case State.Value(currentToken, lastTs) =>
                check(currentToken, token, lastTs, ts) match {
                  case Some(s) => s           -> IO.pure(Some(()))
                  case None    => State.Empty -> IO.pure(None)
                }

              case s @ State.Pending(deferredLastTs) =>
                s -> deferredLastTs.get.map { s => check(s.currentToken, token, s.lastTs, ts).void }

              case s @ State.Empty =>
                s -> IO.pure(None)
            }.flatten
      } yield r
    }

    override def login: IO[String] = IO(logger.info("Login requested")) >> Deferred[IO, State.Value].flatMap {
      deferredValue =>
        state.modify {
          case State.Value(_, _)                 => State.Pending(deferredValue) -> login0(deferredValue)
          case st @ State.Pending(deferredValue) => st                           -> deferredValue.get.map(_.currentToken)
          case State.Empty                       => State.Pending(deferredValue) -> login0(deferredValue)
        }.flatten
    }

    private def login0(deferredLastTs: Deferred[IO, State.Value]): IO[String] = {
      for {
        token <- IO(Random.nextString(8))
        ts    <- clock.monotonic(NANOSECONDS)
        _     <- state.set(State.Value(token, ts))
        _     <- deferredLastTs.complete(State.Value(token, ts))
      } yield token
    }

    private def check(
        token: String,
        receivedToken: String,
        lastReceivedAt: Long,
        receivedAt: Long
    ): Option[State.Value] = {
      if (token === receivedToken && receivedAt < lastReceivedAt + maxInactivity.toNanos) {
        State.Value(token, Math.max(receivedAt, lastReceivedAt)).some
      } else {
        None
      }
    }
  }

  def make(maxInactivity: FiniteDuration)(implicit C: Concurrent[IO], clock: Clock[IO]): IO[Server] = {
    Ref.of[IO, State](State.Empty).map(state => new Live(maxInactivity, state))
  }

  final class LiveFailOnce(maxInactivity: FiniteDuration, state: Ref[IO, State2])(
      implicit C: Concurrent[IO],
      clock: Clock[IO]
  ) extends Server
      with StrictLogging {
    override def request(token: String): IO[Option[Unit]] = {
      for {
        ts <- clock.monotonic(NANOSECONDS)
        r <- state.modify {
              case State2.Value(currentToken, lastTs) =>
                check(currentToken, token, lastTs, ts) match {
                  case Some(s) => s                 -> IO.pure(Some(()))
                  case None    => State2.FailedOnce -> IO.pure(None)
                }

              case s @ State2.Pending(deferredLastTs) =>
                s -> deferredLastTs.get.map { s => check(s.currentToken, token, s.lastTs, ts).void }

              case s @ State2.FailedOnce =>
                s -> IO.pure(None)

              case s @ State2.Empty =>
                s -> IO.pure(None)
            }.flatten
      } yield r
    }

    override def login: IO[String] = IO(logger.info("Login requested")) >> Deferred[IO, State2.Value].flatMap {
      deferredValue =>
        state.modify {
          case State2.Value(_, _)                 => State2.Pending(deferredValue) -> login0(deferredValue)
          case st @ State2.Pending(deferredValue) => st                            -> deferredValue.get.map(_.currentToken)
          case State2.FailedOnce                  => State2.Pending(deferredValue) -> login0(deferredValue)
          case State2.Empty                       => State2.FailedOnce             -> IO.raiseError(new RuntimeException("Login error!"))
        }.flatten
    }

    private def login0(deferredLastTs: Deferred[IO, State2.Value]): IO[String] = {
      for {
        token <- IO(Random.nextString(8))
        ts    <- clock.monotonic(NANOSECONDS)
        _     <- state.set(State2.Value(token, ts))
        _     <- IO(logger.info("Logged in!"))
        _     <- deferredLastTs.complete(State2.Value(token, ts))
      } yield token
    }

    private def check(
        token: String,
        receivedToken: String,
        lastReceivedAt: Long,
        receivedAt: Long
    ): Option[State2.Value] = {
      if (token === receivedToken && receivedAt < lastReceivedAt + maxInactivity.toNanos) {
        State2.Value(token, Math.max(receivedAt, lastReceivedAt)).some
      } else {
        None
      }
    }
  }

  def makeFailOnce(maxInactivity: FiniteDuration)(implicit C: Concurrent[IO], clock: Clock[IO]): IO[Server] = {
    Ref.of[IO, State2](State2.Empty).map(state => new LiveFailOnce(maxInactivity, state))
  }

  sealed abstract class State
  object State {
    final case class Value(currentToken: String, lastTs: Long)   extends State
    final case class Pending(deferredValue: Deferred[IO, Value]) extends State
    case object Empty                                            extends State
  }

  sealed abstract class State2
  object State2 {
    final case class Value(currentToken: String, lastTs: Long)   extends State2
    final case class Pending(deferredValue: Deferred[IO, Value]) extends State2
    case object FailedOnce                                       extends State2
    case object Empty                                            extends State2
  }
}
