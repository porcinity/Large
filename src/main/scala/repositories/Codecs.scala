package repositories

import models.Article
import models.Article.Author
import models.Tag.TagName
import models.User.UserId
import skunk.Codec
import skunk.codec.text.varchar

object Codecs:
  val userId: Codec[UserId] =
    varchar.imap[UserId](UserId.unsafeFrom)(_.value)

  val articleAuthorId: Codec[Author] =
    varchar.imap[Author](Author.unsafeFrom)(_.value)

  val articleId: Codec[Article.Id] =
    varchar.imap[Article.Id](Article.Id.unsafeFrom)(_.value)

  val tagName: Codec[TagName] =
    varchar.imap[TagName](TagName.unsafeFrom)(_.value)