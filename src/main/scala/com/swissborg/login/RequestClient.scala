package com.swissborg.login

import cats.effect.{IO, Timer}
import com.swissborg.login.RequestClient.ClientError

import scala.concurrent.duration._

trait RequestClient {
  def request(token: String): IO[Either[ClientError, Unit]]
}

object RequestClient {
  final class Live(server: Server)(implicit T: Timer[IO]) extends RequestClient {
    override def request(token: String): IO[Either[ClientError, Unit]] =
      server.request(token).map(_.toRight(ClientError.InvalidToken)).delayBy(50.millis)
  }

  sealed abstract class ClientError
  object ClientError {
    case object InvalidToken extends ClientError
    case object Generic      extends ClientError
  }
}
