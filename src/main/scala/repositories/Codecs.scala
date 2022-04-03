package repositories

import models.Blog
import models.Blog.Author
import models.Tag.TagName
import models.User.UserId
import skunk.Codec
import skunk.codec.text.varchar

object Codecs:
  val userId: Codec[UserId] =
    varchar.imap[UserId](UserId.unsafeFrom)(_.value)

  val noteAuthorId: Codec[Author] =
    varchar.imap[Author](Author.unsafeFrom)(_.value)

  val noteId: Codec[Blog.Id] =
    varchar.imap[Blog.Id](Blog.Id.unsafeFrom)(_.value)

  val tagName: Codec[TagName] =
    varchar.imap[TagName](TagName.unsafeFrom)(_.value)