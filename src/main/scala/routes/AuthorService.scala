package routes

import cats.effect.kernel.Outcome.{Errored, Succeeded}
import cats.effect.{Async, Concurrent, Deferred, Sync, Temporal}
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import org.http4s.circe.*
import org.http4s.circe.CirceEntityCodec.circeEntityEncoder
import org.http4s.implicits.*
import org.http4s.syntax.*
import org.http4s.Status.{BadRequest, Created, NoContent, NotFound, Ok, UnprocessableEntity}
import repositories.{Users, AuthorsSkunk}
import models.Author.Codecs.*
import models.Author.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.Monad
import mail.{JavaMailUtil, test}


class AuthorService[F[_]: JsonDecoder: Monad](repository: Users[F], otherRepo: AuthorsSkunk[F]) extends Http4sDsl[F] {

  object AuthorIdVar:
    def unapply(str: String): Option[AuthorId] = Some(AuthorId(str))

  val routes: HttpRoutes[F] = HttpRoutes.of[F] {


    case GET -> Root => Ok(otherRepo.findAllAuthors)
//      for {
//       authors <- Concurrent[F].start(repository.findAllAuthors)
//       fib <- authors.join
//       res <- fib match {
//         case Succeeded(xs) => Ok(xs)
//         case _ => NotFound()
//       }
//      } yield res

    case GET -> Root / AuthorIdVar(id) =>
      for {
//        a <- repository.findAuthorById(id)
        a <- otherRepo.findAuthorById(id)
        res <- a.fold(NotFound())(Ok(_))
      } yield res

    case req @ POST -> Root =>
      for {
        dto <- req.asJsonDecode[AuthorDto]
        a = AuthorDto.toDomain(dto)
        res <- a.fold(UnprocessableEntity(_), x => Ok(repository.create(x)))
      } yield res

    case GET -> Root / AuthorIdVar(id) / "verify" =>
      for {
        author <- repository.findUserById(id.value)
        res <- author.fold(NotFound())(a =>
          val verified = a.copy(
            email = a.email.copy(status = EmailStatus.Verified)
          )
          val updated = repository.update(verified)
          Ok(updated)
        )
      } yield res

    case req @ PUT -> Root / AuthorIdVar(id) =>
      for {
        dto <- req.asJsonDecode[AuthorDto]
        author <- otherRepo.findAuthorById(id)
        res <- author.fold(NotFound())(a =>
          val newInfo = a.copy(
            name = Name(dto.name),
            email = a.email.copy(address = EmailAddress(dto.email))
          )
          val updatedAuthor = repository.update(newInfo)
          Created(updatedAuthor)
        )
      } yield res

    case DELETE -> Root / AuthorIdVar(id) =>
      for {
        res <- repository.delete(id.value)
        y <- res.fold(_ => NotFound(), _ => NoContent())
      } yield y
  }
}
