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
import repositories.{Notes, Users}
import models.User.Codecs.*
import models.User.*
import cats.Monad
import cats.implicits.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import mail.{JavaMailUtil, test}

//import cats.syntax.*

class UserService[F[_]: JsonDecoder: Monad](repository: Users[F]) extends Http4sDsl[F] {

  object UserIdVar:
    def unapply(str: String): Option[UserId] = Some(UserId(str))

  val routes: HttpRoutes[F] = HttpRoutes.of[F] {


    case GET -> Root => Ok(repository.findAllUsers)

    case GET -> Root / UserIdVar(id) =>
      for {
        a <- repository.findUserById(id)
        res <- a.fold(NotFound())(Ok(_))
      } yield res

    case req @ POST -> Root =>
      for {
        dto <- req.asJsonDecode[UserDto]
        a <- UserDto.toDomain(dto).pure[F]
        res <- a.fold(UnprocessableEntity(_), x => Ok(repository.create(x)))
      } yield res

    case GET -> Root / UserIdVar(id) / "verify" =>
      for {
        author <- repository.findUserById(id)
        res <- author.fold(NotFound())(a =>
          val verified = a.copy(
            email = a.email.copy(status = EmailStatus.Verified)
          )
          val updated = repository.update(verified)
          Ok(updated)
        )
      } yield res

    case req @ PUT -> Root / UserIdVar(id) =>
      for {
        dto <- req.asJsonDecode[UserDto]
        author <- repository.findUserById(id)
        res <- author.fold(NotFound())(a =>
          val newInfo = a.copy(
            name = Name(dto.name),
            email = a.email.copy(address = EmailAddress(dto.email))
          )
          val updatedAuthor = repository.update(newInfo)
          Created(updatedAuthor)
        )
      } yield res

    case DELETE -> Root / UserIdVar(id) =>
      for {
        res <- repository.delete(id)
        y <- res.fold(NotFound())( _ => NoContent())
      } yield y
  }
}
