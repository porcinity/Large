package repositories

import models.Blog.{BlogAuthor, BlogId}
import models.Tag.TagName
import models.User.UserId
import skunk.Codec
import skunk.codec.text.varchar

object Codecs:
  val userId: Codec[UserId] =
    varchar.imap[UserId](UserId(_))(_.value)

  val blogAuthorId: Codec[BlogAuthor] =
    varchar.imap[BlogAuthor](BlogAuthor(_))(_.value)

  val blogId: Codec[BlogId] =
    varchar.imap[BlogId](BlogId(_))(_.value)

  val tagName: Codec[TagName] =
    varchar.imap[TagName](TagName(_))(_.value)
