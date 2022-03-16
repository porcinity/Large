package models

import doobie.Read
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

  opaque type KewlContent = String

  object KewlContent:
    def apply(value: String): KewlContent = value

  case class KewlBlog(id: KewlId, title: KewlTitle, content: KewlContent)

  object KewlBlog:
    implicit val kewlCodec: Codec[KewlBlog] = deriveCodec[KewlBlog]
    implicit val kewlBlogRead: Read[KewlBlog] =
      Read[(Int, String, String)].map { case (id, title, content) => KewlBlog(KewlId(id), KewlTitle(title), KewlContent(content)) }
