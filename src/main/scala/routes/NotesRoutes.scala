package routes

import cats.data.Validated.{Invalid, Valid}
import cats.effect.Concurrent
import models.Note.*
import models.Tag.{TagDto, TagName}
import org.http4s.*
import org.http4s.Status.{Created, NoContent, NotFound, Ok, UnprocessableEntity}
import org.http4s.circe.*
import org.http4s.circe.CirceEntityCodec.circeEntityEncoder
import org.http4s.dsl.Http4sDsl
import org.http4s.implicits.*
import org.http4s.syntax.*
import repositories.Notes

// These are necessary to use for-comprehensions on F
import cats.syntax.flatMap.*
import cats.syntax.functor.*
//import cats.syntax.*
import cats.implicits.*


//import monocle.syntax.*
import monocle.Lens
import monocle.macros.GenLens
import monocle.macros.syntax.AppliedFocusSyntax
import monocle.syntax.all.*

// The type constraint of Concurrent is necessary to decode Json
class NotesRoutes[F[_]: Concurrent](repository: Notes[F]) extends Http4sDsl[F] {

  implicit val tagCoder: QueryParamDecoder[TagName] =
    QueryParamDecoder[String].map(TagName.apply)

  object BlogIdVar:
    def unapply(str: String): Option[NoteId] = Some(NoteId(str))


  object OptionalTagQueryParamMatcher extends  OptionalQueryParamDecoderMatcher[TagName]("tag")
  object OptionalUserIdParamMatch extends OptionalQueryParamDecoderMatcher[String]("user")

  val routes: HttpRoutes[F] = HttpRoutes.of[F] {

    case GET -> Root :? OptionalTagQueryParamMatcher(tag) +& OptionalUserIdParamMatch(user) => (tag, user) match {
      case (Some(t), Some(u)) => Ok(repository.findNoteByTagAndUser(t, u))
      case (Some(t), None) => Ok(repository.findNoteByTag(t))
      case (None, Some(u)) => Ok(repository.findNoteByUser(u))
      case (None, None) => Ok(repository.findAllNotes)
    }

    case GET -> Root / BlogIdVar(id) =>
      for {
        blog <- repository.findNoteById(id)
        res <- blog.fold(NotFound())(Ok(_))
      } yield res

    case req @ POST -> Root =>
      for
        dto <- req.decodeJson[NoteDto]
        blog <- NoteDto.toDomain(dto).pure[F]
        res <- blog.fold(UnprocessableEntity(_), b => Created(repository.create(b)))
      yield res

    case req @ POST -> Root / BlogIdVar(id) / "addTag" =>
      for {
        dto <- req.asJsonDecode[TagDto]
        tag <- TagDto.toDomain(dto, id).pure[F]
        res <- Created(repository.addTag(tag))
      } yield res


    case req @ PUT -> Root / BlogIdVar(id) =>
      for {
        dto <- req.decodeJson[NoteDto]
        foundBlog <- repository.findNoteById(id)
        updatedBlog = NoteDto.toDomain(dto)
        res <- (foundBlog, updatedBlog) match
          case (None, _) => NotFound()
          case (_, Invalid(e)) => UnprocessableEntity(e)
          case (Some(b), Valid(u)) =>
            val newBlog = b.copy(title = u.title, content = u.content)
            Created(repository.update(newBlog))
      } yield res

    case DELETE -> Root / BlogIdVar(id) =>
      for {
        res <- repository.delete(id)
        y <- res.fold(NotFound())( _ => NoContent())
      } yield y
  }
}