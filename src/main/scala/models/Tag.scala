package models

import models.Blog.*
import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import com.aventrix.jnanoid.jnanoid.*

import scala.annotation.targetName

object Tag:
  implicit val tagCodec: Codec[Tag] = deriveCodec[Tag]
  opaque type TagId = String
  object TagId:
    def apply(value: String): TagId = value

  opaque type TagName = String
  object TagName:
    def apply(value: String): TagName = value

  opaque type TaggedBlog = String
  object TaggedBlog:
    def apply(value: String): TaggedBlog = value

  extension (x: TagId)
    @targetName("value_TagId")
    def value: String = x

  extension (x: TagName)
    @targetName("value_TagName")
    def value: String = x

  extension (x: TaggedBlog)
    @targetName("value_blogId")
    def value: String = x

  case class Tag(id: TagId, name: TagName, blogId: TaggedBlog)

  case class TagDto(name: String)
  object TagDto:
    implicit val tagDtoCodec: Codec[TagDto] = deriveCodec[TagDto]
    def toDomain(dto: TagDto, id: BlogId): Tag =
      val tagId = NanoIdUtils.randomNanoId()
      Tag(
        TagId(tagId),
        TagName(dto.name),
        TaggedBlog(id.value)
      )
