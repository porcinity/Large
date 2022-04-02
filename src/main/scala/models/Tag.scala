package models

import cats.data.NonEmptyChain
import models.Note.Id
import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import com.aventrix.jnanoid.jnanoid.*
import eu.timepit.refined.api.RefinedTypeOps
import eu.timepit.refined.cats.CatsRefinedTypeOpsSyntax
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.refined.*
import cats.syntax.all.*

object Tag {

  case class Tag(id: TagId, name: TagName, noteId: TaggedNote)
  implicit val tagCodec: Codec[Tag] = deriveCodec

  type TagId = NonEmptyString
  object TagId extends RefinedTypeOps[TagId, String] with CatsRefinedTypeOpsSyntax

  type TagName = NonEmptyString
  object TagName extends RefinedTypeOps[TagName, String] with CatsRefinedTypeOpsSyntax

  type TaggedNote = NonEmptyString
  object TaggedNote extends RefinedTypeOps[TaggedNote, String] with CatsRefinedTypeOpsSyntax

  case class TagDto(name: String)

  object TagDto:
    implicit val tagDtoCodec: Codec[TagDto] = deriveCodec[TagDto]

    def toDomain(dto: TagDto, id: Note.Id): Either[NonEmptyChain[String], Tag] =
      val tagId = NanoIdUtils.randomNanoId()
      (
        TagId.from(tagId).toEitherNec,
        TagName.from(dto.name).toEitherNec,
        TaggedNote.from(id.value).toEitherNec
      ).parMapN(Tag.apply)
}