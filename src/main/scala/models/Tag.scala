package models

import models.Note.*
import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import com.aventrix.jnanoid.jnanoid.*

import scala.annotation.targetName

object Tag {

  case class Tag(id: TagId, name: TagName, noteId: TaggedNote)

  implicit val tagCodec: Codec[Tag] = deriveCodec[Tag]
  opaque type TagId = String

  object TagId:
    def apply(value: String): TagId = value

  opaque type TagName = String

  object TagName:
    def apply(value: String): TagName = value
  extension (x: TagName)
    @targetName("value_TagName")
    def value: String = x

  opaque type TaggedNote = String

  object TaggedNote:
    def apply(value: String): TaggedNote = value

  extension (x: TagId)
    @targetName("value_TagId")
    def value: String = x

  extension (x: TaggedNote)
    @targetName("value_TaggedNote")
    def value: String = x

  case class TagDto(name: String)

  object TagDto:
    implicit val tagDtoCodec: Codec[TagDto] = deriveCodec[TagDto]

    def toDomain(dto: TagDto, id: NoteId): Tag =
      val tagId = NanoIdUtils.randomNanoId()
      Tag(
        TagId(tagId),
        TagName(dto.name),
        TaggedNote(id.value)
      )
}