package routes

import cats.effect.kernel.Outcome.{Errored, Succeeded}
import cats.effect.{Async, Concurrent, Deferred, Resource, Sync, Temporal}
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
import cats.data.Validated.Valid
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Json
import mail.{JavaMailUtil, test}
import models.Note.NoteDto
import io.circe.syntax.*
import monocle.syntax.all.*
import monocle.refined.all.*
//import cats.syntax.*


import UpdateUser.GenericDerivation.*
class UserService[F[_]: JsonDecoder: Monad](repository: Users[F]) extends Http4sDsl[F] {

  object UserIdVar:
    def unapply(str: String): Option[UserId] = Some(UserId.unsafeFrom(str))

  val routes: HttpRoutes[F] = HttpRoutes.of[F] {


    case GET -> Root =>
      for {
        users <- repository.findAllUsers
        res <- Ok(ListOfUsers(users))
      } yield res

    case GET -> Root / UserIdVar(id) =>
      for {
        a <- repository.findUserById(id)
        res <- a.fold(NotFound())(Ok(_))
      } yield res

    case req @ POST -> Root =>
      for {
        dto <- req.asJsonDecode[UserDto]
        a <- UserDto.toDomain(dto).pure[F]
        _ <- JavaMailUtil.main(Array("")).pure[F]
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

    case req @ POST -> Root / UserIdVar(id) / "addNote" =>
      for {
        dto <- req.asJsonDecode[NoteDto]

      } yield ???

    case req @ PUT -> Root / UserIdVar(id) =>
      for {
        dto <- req.asJsonDecode[UpdateUser]
        user <- repository.findUserById(id)
        res <- user.fold(NotFound())(u => {
          dto match
            case UpdateNameAndEmail(name, email) => ???
            case UpdateName(n) => {
              val up = u.focus(_.name).replace(n)
              Created(repository.update(up))
            }
            case UpdateEmail(e) =>
              val up = u.focus(_.email.address).replace(e)
              Created(repository.update(up))
        })
      } yield res

    case DELETE -> Root / UserIdVar(id) =>
      for {
        res <- repository.delete(id)
        y <- res.fold(NotFound())( _ => NoContent())
      } yield y
  }
}
