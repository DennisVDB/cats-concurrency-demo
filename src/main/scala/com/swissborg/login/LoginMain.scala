package com.swissborg.login

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import com.typesafe.scalalogging.LazyLogging

import scala.concurrent.duration._

object LoginMain extends IOApp with LazyLogging {

  override def run(args: List[String]): IO[ExitCode] = {
    /* Simple */
    val simpleClient = for {
      server       <- Server.make(1.second)
      loginClient  = new LoginClient.Live(server)
      authedClient = new RequestClient.Live(server)
    } yield AuthedRequestClient.simple(loginClient, authedClient)

    /* Ref + Deferred */
    val client1 = for {
      server       <- Server.make(1.second)
      loginClient  = new LoginClient.Live(server)
      authedClient = new RequestClient.Live(server)
      tokenCache   <- TokenCache.make(loginClient)
    } yield AuthedRequestClient.make(tokenCache, authedClient)

    /* Ref + Deferred with broken login */
    val client1Fail = for {
      server       <- Server.makeFailOnce(1.second)
      loginClient  = new LoginClient.Live(server)
      authedClient = new RequestClient.Live(server)
      tokenCache   <- TokenCache.make(loginClient)
    } yield AuthedRequestClient.make(tokenCache, authedClient)

    /* Ref + Deferred with failed login handling */
    val client2 = for {
      server       <- Server.make(1.second)
      loginClient  = new LoginClient.Live(server)
      authedClient = new RequestClient.Live(server)
      tokenCache   <- TokenCache.make2(loginClient)
    } yield AuthedRequestClient.make(tokenCache, authedClient)

    /* Ref + Deferred with failed login handling awefawef*/
    val client2Fail = for {
      server       <- Server.makeFailOnce(1.second)
      loginClient  = new LoginClient.Live(server)
      authedClient = new RequestClient.Live(server)
      tokenCache   <- TokenCache.make2(loginClient)
    } yield AuthedRequestClient.make(tokenCache, authedClient)

    for {
      client <- client2Fail
      _ <- List
            .fill(4)(client.request.attempt.flatMap {
              case Right(_)  => IO.unit
              case Left(err) => IO(logger.error("{}", err.getMessage))
            })
            .parSequence_
      _ <- IO.sleep(2.seconds)
      _ <- IO(println(""))
      _ <- List
            .fill(4)(client.request.attempt.flatMap {
              case Right(_)  => IO.unit
              case Left(err) => IO(logger.error("{}", err.getMessage))
            })
            .parSequence_
      _ <- IO.sleep(2.seconds)
      _ <- IO(println(""))
      _ <- List
            .fill(4)(client.request.attempt.flatMap {
              case Right(_)  => IO.unit
              case Left(err) => IO(logger.error("{}", err.getMessage))
            })
            .parSequence_
    } yield ExitCode.Success
  }

}
