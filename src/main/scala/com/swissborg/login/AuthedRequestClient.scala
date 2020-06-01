package com.swissborg.login

import cats.effect.IO
import com.swissborg.login.RequestClient.ClientError

trait AuthedRequestClient {
  def request: IO[Either[ClientError, Unit]]
}

object AuthedRequestClient {
  def simple(loginClient: LoginClient, authedClient: RequestClient): AuthedRequestClient = new AuthedRequestClient {
    override val request: IO[Either[ClientError, Unit]] = {
      for {
        token <- loginClient.login
        r     <- authedClient.request(token)
      } yield r
    }
  }

  def make(tokenCache: TokenCache, authedClient: RequestClient): AuthedRequestClient = new AuthedRequestClient {
    override val request: IO[Either[ClientError, Unit]] = {
      val firstAttempt = for {
        token <- tokenCache.token
        r     <- authedClient.request(token)
      } yield r

      firstAttempt.flatMap {
        case Left(ClientError.InvalidToken) =>
          for {
            token <- tokenCache.refreshToken
            r     <- authedClient.request(token)
          } yield r

        case other =>
          IO.pure(other)
      }
    }
  }
}
