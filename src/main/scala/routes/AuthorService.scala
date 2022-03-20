package routes

import cats.effect.Concurrent
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import org.http4s.circe.*
import org.http4s.circe.CirceEntityCodec.circeEntityEncoder
import org.http4s.implicits.*
import org.http4s.syntax.*
import org.http4s.Status.{Created, NoContent, NotFound, Ok}
import repositories.Authors
import models.Author.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import mail.JavaMailUtil
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
        dto <- req.decodeJson[AuthorDto]
        a = AuthorDto.toDomain(dto)
        x <- repository.create(a)
//        ignore = JavaMailUtil.sendMail(a.email.address.value, a.id.value)
        res <- Created(x)
      } yield res

    case GET -> Root / IntVar(id) / "verify" =>
      for {
        author <- repository.findAuthorById(id)
        res <- author.fold(NotFound())(a =>
          val verified = a.copy(
            email = a.email.copy(status = EmailStatus.Verified)
          )
          val updated = repository.update(verified)
          Ok(updated)
        )
      } yield res

    case req @ PUT -> Root / IntVar(id) =>
      for {
        dto <- req.decodeJson[AuthorDto]
        author <- repository.findAuthorById(id)
        res <- author.fold(NotFound())(a =>
          val newInfo = a.copy(
            name = Name(dto.name),
            email = a.email.copy(address = EmailAddress(dto.email))
          )
          val updatedAuthor = repository.update(newInfo)
          Created(updatedAuthor)
        )
      } yield res

    case DELETE -> Root / IntVar(id) =>
      for {
        res <- repository.delete(id)
        y <- res.fold(_ => NotFound(), _ => NoContent())
      } yield y
  }
}
