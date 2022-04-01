package repositories

import models.Note
import models.Note.Author
import models.Tag.TagName
import models.User.UserId
import skunk.Codec
import skunk.codec.text.varchar

object Codecs:
  val userId: Codec[UserId] =
    varchar.imap[UserId](UserId.unsafeFrom)(_.value)

  val blogAuthorId: Codec[Author] =
    varchar.imap[Author](Author.unsafeFrom)(_.value)

  val noteId: Codec[Note.Id] =
    varchar.imap[Note.Id](Note.Id.unsafeFrom)(_.value)

  val tagName: Codec[TagName] =
    varchar.imap[TagName](TagName(_))(_.value)