package com.swissborg.login

import cats.effect.{IO, Timer}

import scala.concurrent.duration._

trait LoginClient {
  def login: IO[String]
}

object LoginClient {
  final class Live(server: Server)(implicit T: Timer[IO]) extends LoginClient {
    override def login: IO[String] = server.login.delayBy(50.millis)
  }
}
