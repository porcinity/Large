package repositories

import models.Note.{NoteAuthor, NoteId}
import models.Tag.TagName
import models.User.UserId
import skunk.Codec
import skunk.codec.text.varchar

object Codecs:
  val userId: Codec[UserId] =
    varchar.imap[UserId](UserId.unsafeFrom)(_.value)

  val blogAuthorId: Codec[NoteAuthor] =
    varchar.imap[NoteAuthor](NoteAuthor(_))(_.value)

  val noteId: Codec[NoteId] =
    varchar.imap[NoteId](NoteId(_))(_.value)

  val tagName: Codec[TagName] =
    varchar.imap[TagName](TagName(_))(_.value)