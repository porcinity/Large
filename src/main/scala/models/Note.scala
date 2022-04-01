package models

import doobie.{Read, Write}
import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec

import java.time.LocalDate
import doobie.implicits.legacy.localdate.*
import com.aventrix.jnanoid.jnanoid.*
import cats.data.*
import cats.implicits.*
import eu.timepit.refined.api.RefinedTypeOps
import eu.timepit.refined.cats.CatsRefinedTypeOpsSyntax
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.refined.*


object Note:
  case class Note(id: Id, title: Title, content: Content, author: Author)
  implicit val noteCodec: Codec[Note] = deriveCodec[Note]

  type Id = NonEmptyString
  object Id extends RefinedTypeOps[NonEmptyString, String] with CatsRefinedTypeOpsSyntax

  type Title = NonEmptyString
  object Title extends RefinedTypeOps[NonEmptyString, String] with CatsRefinedTypeOpsSyntax

  type Content = NonEmptyString
  object Content extends RefinedTypeOps[NonEmptyString, String] with CatsRefinedTypeOpsSyntax

  type Author = NonEmptyString
  object Author extends RefinedTypeOps[NonEmptyString, String] with CatsRefinedTypeOpsSyntax

  case class NoteDto(title: String, content: String, author: String)

  object NoteDto:
    implicit val noteDtoCodec: Codec[NoteDto] = deriveCodec[NoteDto]
    def toDomain(dto: NoteDto): Either[NonEmptyChain[String], Note] =
      val id = NanoIdUtils.randomNanoId()
      (Id.from(id).toEitherNec,
        Title.from(dto.title).toEitherNec,
        Content.from(dto.content).toEitherNec,
        Author.from(dto.author).toEitherNec
        ).mapN(Note.apply)