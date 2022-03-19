package routes

import cats.effect.Concurrent
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import org.http4s.circe.*
import org.http4s.circe.CirceEntityCodec.circeEntityEncoder
import org.http4s.implicits.*
import org.http4s.syntax.*
import repositories.Authors
import models.Author.*

import cats.syntax.flatMap.*
import cats.syntax.functor.*
class AuthorService[F[_]: Concurrent](repository: Authors[F]) extends Http4sDsl[F] {
  val routes: HttpRoutes[F] = HttpRoutes.of[F] {

    case GET -> Root => Ok(repository.findAllAuthors)

    case GET -> Root / IntVar(id) =>
      for {
        a <- repository.findAuthorById(id)
        res <- a.fold(NotFound())(Ok(_))
      } yield res

    case req @ POST -> Root =>
      for {
        author <- req.decodeJson[Author]
        a <- repository.create(author)
        res <- Created(a)
      } yield res
  }
}
