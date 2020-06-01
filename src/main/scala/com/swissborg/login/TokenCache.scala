package com.swissborg.login

import cats.effect.concurrent.{Deferred, Ref}
import cats.effect.{Concurrent, IO}
import cats.implicits._
import com.typesafe.scalalogging.{LazyLogging, StrictLogging}

trait TokenCache {
  def token: IO[String]
  def refreshToken: IO[String]
}

object TokenCache {
  final class Live(loginClient: LoginClient, state: Ref[IO, State])(implicit C: Concurrent[IO])
      extends TokenCache
      with LazyLogging {
    override val token: IO[String] = Deferred[IO, String].flatMap { waitForToken =>
      state.modify {
        case s @ State.Idle(token)           => s                           -> IO.pure(token)
        case s @ State.Waiting(waitForToken) => s                           -> waitForToken.get
        case State.Empty                     => State.Waiting(waitForToken) -> refreshToken0(waitForToken)
      }.flatten
    }

    override val refreshToken: IO[String] = Deferred[IO, String].flatMap { waitForToken =>
      state.modify {
        case State.Idle(_)               => State.Waiting(waitForToken) -> refreshToken0(waitForToken)
        case State.Waiting(waitForToken) => State.Waiting(waitForToken) -> waitForToken.get
        case State.Empty                 => State.Waiting(waitForToken) -> refreshToken0(waitForToken)
      }.flatten
    }

    private def refreshToken0(waitForToken: Deferred[IO, String]): IO[String] = {
      for {
        token <- loginClient.login
        _     <- state.set(State.Idle(token))
        _     <- waitForToken.complete(token)
      } yield token
    }
  }

  def make(loginClient: LoginClient)(implicit C: Concurrent[IO]): IO[TokenCache] = {
    Ref.of[IO, State](State.Empty).map(state => new Live(loginClient, state))
  }

  final class Live2(loginClient: LoginClient, state: Ref[IO, State2])(implicit C: Concurrent[IO])
      extends TokenCache
      with StrictLogging {
    override val token: IO[String] = Deferred[IO, Either[Throwable, String]].flatMap { waitForToken =>
      state.modify {
        case s @ State2.Idle(token)           => s                            -> IO.pure(token)
        case s @ State2.Waiting(waitForToken) => s                            -> waitForToken.get.rethrow
        case State2.Empty                     => State2.Waiting(waitForToken) -> refreshToken0(waitForToken).rethrow
      }.flatten
    }

    override val refreshToken: IO[String] = Deferred[IO, Either[Throwable, String]].flatMap { waitForToken =>
      state.modify {
        case State2.Idle(_)               => State2.Waiting(waitForToken) -> refreshToken0(waitForToken).rethrow
        case State2.Waiting(waitForToken) => State2.Waiting(waitForToken) -> waitForToken.get.rethrow
        case State2.Empty                 => State2.Waiting(waitForToken) -> refreshToken0(waitForToken).rethrow
      }.flatten
    }

    private def refreshToken0(waitForToken: Deferred[IO, Either[Throwable, String]]): IO[Either[Throwable, String]] = {
      for {
        token <- loginClient.login.attempt
        _ <- token match {
              case Right(token) => state.set(State2.Idle(token))
              case Left(_)      => state.set(State2.Empty)
            }
        _ <- waitForToken.complete(token)
      } yield token
    }
  }

  def make2(loginClient: LoginClient)(implicit C: Concurrent[IO]): IO[TokenCache] = {
    Ref.of[IO, State2](State2.Empty).map(state => new Live2(loginClient, state))
  }

  sealed abstract class State
  object State {
    final case class Idle(token: String)                         extends State
    final case class Waiting(waitForToken: Deferred[IO, String]) extends State
    case object Empty                                            extends State
  }

  sealed abstract class State2
  object State2 {
    final case class Idle(token: String)                                            extends State2
    final case class Waiting(waitForToken: Deferred[IO, Either[Throwable, String]]) extends State2
    case object Empty                                                               extends State2
  }
}
