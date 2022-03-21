package models

import doobie.{Read, Write}
import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec

object Blog:
  case class Blog(id: BlogId, title: BlogTitle, content: BlogContent)

  opaque type BlogId = Int

  object BlogId:
    def apply(value: Int): BlogId = value

  extension (x: BlogId)
    def value: Int = x

  opaque type BlogTitle = String

  object BlogTitle:
    def apply(value: String): BlogTitle = value

  extension (x: BlogTitle)
    def value: String = x

  opaque type BlogContent = String

  def BlogContent(value: String): BlogContent = value

  extension (x: BlogContent) def v: String = x


  implicit val kewlCodec: Codec[Blog] = deriveCodec[Blog]
  implicit val kewlBlogRead: Read[Blog] =
    Read[(Int, String, String)].map { case (id, title, content) =>
      Blog(BlogId(id), BlogTitle(title), BlogContent(content))
    }
  implicit val kewlBlogWrite: Write[Blog] =
    Write[(Int, String, String)].contramap { blog =>
      (blog.id.value, blog.title.value, blog.content.v)
    }
    
  case class KewlBlogDto(title: String, content: String)
  
  object KewlBlogDto:
    implicit val dtoCodec: Codec[KewlBlogDto] = deriveCodec[KewlBlogDto]
    def toDomain(dto: KewlBlogDto): Blog =
      val r = scala.util.Random.nextInt(99999)
      Blog(BlogId(r),BlogTitle(dto.title), BlogContent(dto.content))