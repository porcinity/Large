package io.porcinity.large.api

import cats.Monad
import cats.effect.Concurrent
import io.porcinity.large.domain.Tag.*
import io.porcinity.large.domain.Tag.TagDto.*
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import org.http4s.circe.*
import org.http4s.circe.CirceEntityCodec.circeEntityEncoder
import org.http4s.*
import org.http4s.Status.*
import org.http4s.implicits.*
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import io.circe.parser.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import io.porcinity.large.common.GetItems
import io.porcinity.large.persistence.Tags

class TagsRoutes[F[_]: Concurrent](repository: Tags[F]) extends Http4sDsl[F]:

  val routes: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root =>
      for {
        tags <- repository.findAllTags
        res <- Ok(GetItems(tags))
      } yield res

    case req @ POST -> Root =>
      for {
        dto <- req.asJsonDecode[TagDto]
        res <- Ok(repository.createTag(dto.name))
      } yield res
  }
