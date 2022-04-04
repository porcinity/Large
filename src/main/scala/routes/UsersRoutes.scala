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
import repositories.{Articles, Users}
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
import models.Article.ArticleDto
import io.circe.syntax.*
import monocle.syntax.all.*
import monocle.refined.all.*
import UpdateUser.GenericDerivation.*
import common.*
class UsersRoutes[F[_]: JsonDecoder: Monad](repository: Users[F]) extends Http4sDsl[F] {

  object UserIdVar:
    def unapply(str: String): Option[UserId] = Some(UserId.unsafeFrom(str))

  val routes: HttpRoutes[F] = HttpRoutes.of[F] {

    case GET -> Root =>
      for {
        users <- repository.findAllUsers
        res <- Ok(GetItems(users))
      } yield res

    case GET -> Root / UserIdVar(id) =>
      for {
        u <- repository.findUserById(id)
        res <- u.fold(NotFound())(u => Ok(GetItem(u)))
      } yield res

    case req @ POST -> Root =>
      for {
        dto <- req.asJsonDecode[UserDto]
        u <- UserDto.toDomain(dto).pure[F]
        res <- u.fold(e => UnprocessableEntity(GetItems(e.toList)),
                  x => Ok(repository.create(x).map(GetItem.apply)))
      } yield res

    case GET -> Root / UserIdVar(id) / "verify" =>
      for {
        u <- repository.findUserById(id)
        res <- u.fold(NotFound())(x =>
          val verified = x.focus(_.email.status).replace(EmailStatus.Verified)
          Ok(repository.update(verified))
        )
      } yield res

    case GET -> Root / UserIdVar(id) / "followers" =>
      for {
        res <- Ok(repository.findFollowers(id).map(GetItems.apply))
      } yield res

    case GET -> Root / UserIdVar(id) / "following" =>
      for {
        res <- Ok(repository.findFollowing(id).map(GetItems.apply))
      } yield res

    case req @ POST -> Root / UserIdVar(id) / "follow" =>
      for {
        dto <- req.asJsonDecode[FollowUserDto]
        res <- Ok(repository.followUser(id, dto.asUser))
      } yield res

    case req @ DELETE -> Root / UserIdVar(id) / "follow" =>
      for {
        dto <- req.asJsonDecode[FollowUserDto]
        res <- Ok(repository.unfollowUser(id, dto.asUser))
      } yield res

    case req @ POST -> Root / UserIdVar(id) / "addArticle" =>
      for {
        dto <- req.asJsonDecode[ArticleDto]

      } yield ???

    case req @ PUT -> Root / UserIdVar(id) =>
      for {
        dto <- req.asJsonDecode[UpdateUser]
        user <- repository.findUserById(id)
        res <- user.fold(NotFound())(u => {
          val up = UpdateUser.of(dto, u)
          up.fold(UnprocessableEntity(_), u => Created(repository.update(u).map(GetItem.apply)))
        })
      } yield res

    case DELETE -> Root / UserIdVar(id) =>
      for {
        d <- repository.delete(id)
        res <- d.fold(NotFound())(_ => NoContent())
      } yield res
  }
}
