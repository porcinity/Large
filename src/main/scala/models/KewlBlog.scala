package models

import doobie.{Read, Write}
import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec

object KewlBlog:
  case class KewlBlog(id: KewlId, title: KewlTitle, content: KewlContent)

  opaque type KewlId = Int

  object KewlId:
    def apply(value: Int): KewlId = value

  extension (x: KewlId)
    def value: Int = x

  opaque type KewlTitle = String

  object KewlTitle:
    def apply(value: String): KewlTitle = value

  extension (x: KewlTitle)
    def value: String = x

  opaque type KewlContent = String

  def KewlContent(value: String): KewlContent = value

  extension (x: KewlContent) def v: String = x


  implicit val kewlCodec: Codec[KewlBlog] = deriveCodec[KewlBlog]
  implicit val kewlBlogRead: Read[KewlBlog] =
    Read[(Int, String, String)].map { case (id, title, content) =>
      KewlBlog(KewlId(id), KewlTitle(title), KewlContent(content))
    }
  implicit val kewlBlogWrite: Write[KewlBlog] =
    Write[(Int, String, String)].contramap { kewlblog =>
      (kewlblog.id.value, kewlblog.title.value, kewlblog.content.v)
    }
    
  case class KewlBlogDto(title: String, content: String)
  
  object KewlBlogDto:
    implicit val dtoCodec: Codec[KewlBlogDto] = deriveCodec[KewlBlogDto]
    def toDomain(dto: KewlBlogDto): KewlBlog =
      val r = scala.util.Random.nextInt(99999)
      KewlBlog(KewlId(r),KewlTitle(dto.title), KewlContent(dto.content))