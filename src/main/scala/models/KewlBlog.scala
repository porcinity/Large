package models

import doobie.{Read, Write}
import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec

object KewlBlog:
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

  case class KewlBlog(id: KewlId, title: KewlTitle, content: KewlContent)

  implicit val kewlCodec: Codec[KewlBlog] = deriveCodec[KewlBlog]
  implicit val kewlBlogRead: Read[KewlBlog] =
    Read[(Int, String, String)].map { case (id, title, content) =>
      KewlBlog(KewlId(id), KewlTitle(title), KewlContent(content)) }
  implicit val kewlBlogWrite: Write[KewlBlog] =
    Write[(Int, String, String)].contramap { kewlblog =>
      (kewlblog.id.value, kewlblog.title.value, kewlblog.content.v)
    }