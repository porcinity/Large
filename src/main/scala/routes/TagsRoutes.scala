package routes

import cats.Monad
import cats.effect.Concurrent
import models.Tag.*
import models.Tag.TagDto.*
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import repositories.Tags
import org.http4s.circe.CirceEntityCodec.circeEntityEncoder
import org.http4s.circe.*
import org.http4s.*
import org.http4s.Status.*
import org.http4s.implicits.*
import cats.syntax.all.*

class TagsRoutes[F[_]: Concurrent](repository: Tags[F]) extends Http4sDsl[F]:

  val routes: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root => Ok(repository.findAllTags)
    case req @ POST -> Root =>
      for {
        dto <- req.asJsonDecode[TagDto]
        res <- Ok(repository.createTag(dto.name))
      } yield res
  }
