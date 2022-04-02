package repositories

import cats.effect.{Concurrent, Resource}
import models.Tag.*
import models.Note.*
import Codecs.*
import skunk.*
import skunk.implicits.*
import skunk.codec.text.*
import skunk.codec.temporal.*
import cats.syntax.all.*
import skunk.codec.all.int8

trait Notes[F[_]]:
  def findAllNotes: F[List[Note]]
  def findNoteById(id: Id): F[Option[Note]]
  def findNoteByUser(user: String): F[List[Note]]
  def findNoteByTag(tagName: TagName): F[List[Note]]
  def findNoteByTagAndUser(tagName: TagName, user: String): F[List[Note]]
  def create(note: Note): F[Note]
  def update(note: Note): F[Note]
  def delete(noteId: Id): F[Option[Note]]
  def addTag(tag: Tag): F[Tag]

object Notes:
  import NotesSql.*
  def make[F[_]: Concurrent](postgres: Resource[F, Resource[F, Session[F]]]): Notes[F] =
    new Notes[F] {
      override def findAllNotes: F[List[Note]] = postgres.use(_.use(_.execute(selectAll)))

      override def findNoteById(id: Id): F[Option[Note]] = postgres.use(_.use { session =>
        session.prepare(selectById).use { ps =>
          ps.option(id)
        }
      })

      override def findNoteByUser(user: String): F[List[Note]] = postgres.use(_.use { session =>
        session.prepare(selectByUser).use { ps =>
          ps.stream(user, 15).compile.toList
        }
      })

      override def findNoteByTag(tagName: TagName): F[List[Note]] = postgres.use(_.use { session =>
        session.prepare(selectByTag).use { ps =>
          ps.stream(tagName, 15).compile.toList
        }
      })

      override def findNoteByTagAndUser(tagName: TagName, user: String): F[List[Note]] = postgres.use(_.use { session =>
        session.prepare(selectByTagAndUser).use { ps =>
          ps.stream((tagName, user),15).compile.toList
        }
      })

      override def create(note: Note): F[Note] = postgres.use(_.use { session =>
        session.prepare(insertNote).use(_.execute(note)).as(note)
      })

      override def update(note: Note): F[Note] = postgres.use(_.use { session =>
        session.prepare(updateNote).use(_.execute(note)).as(note)
      })

      override def delete(noteId: Id): F[Option[Note]] = postgres.use(_.use { session =>
        session.prepare(deleteNote).use(ps => ps.option(noteId))
      })

      override def addTag(tag: Tag): F[Tag] = postgres.use(_.use { session =>
        session.prepare(insertTag).use(_.execute(tag)).as(tag)
      })
    }

private object NotesSql:
  val decoder: Decoder[Note] =
    ( noteId ~ varchar ~ varchar ~ noteAuthorId ~ int8 ~ varchar ~ date ~ date ).map {
      case nId ~ title ~ content ~ aId ~ likes ~ vis ~ publish ~ edit =>
        Note(
          nId,
          Title.unsafeFrom(title),
          Content.unsafeFrom(content),
          aId,
          WordCount.unsafeFrom(content.length),
          ReadingTime.unsafeFrom(content.length / 200.0),
          Likes.unsafeFrom(likes.toInt),
          Visibility.fromString(vis),
          BlogDate(publish),
          BlogDate(edit)
        )
    }

  val encoder: Encoder[Note] =
    (
      varchar ~ varchar ~ varchar ~ varchar ~ varchar ~ date ~ date
    ).contramap { case Note(id, title, content, author, _, _, _, visibility, publish, edit) =>
      id.value ~ title.value ~ content.value ~ author.value ~ visibility.toString ~ publish.value ~ edit.value
    }

  val tagEncoder: Encoder[Tag] =
    ( varchar ~ varchar ~ varchar).contramap { t => t.id.value ~ t.name.value ~ t.noteId.value }

  val selectAll: Query[Void, Note] =
    sql"""
         select b.note_id,
                b.note_title,
                b.note_content,
                b.note_author,
                (select count(*) from likes_map l where l.like_blog = b.note_id) as likes,
                b.note_visibility,
                b.note_publish_date,
                b.note_last_edit_date
         from notes b;
    """.query(decoder)

  val selectById: Query[Id, Note] =
    sql"""
         select b.note_id,
                b.note_title,
                b.note_content,
                b.note_author,
                (select count(*) from likes_map l where l.like_blog = b.note_id) as likes,
                b.note_visibility,
                b.note_publish_date,
                b.note_last_edit_date
         from notes b
        where b.note_id = $noteId;
    """.query(decoder)

  val insertNote: Command[Note] =
    sql"""
        insert into notes(note_id, note_title, note_content, note_author, note_visibility, note_publish_date, note_last_edit_date)
        values ($varchar, $varchar, $varchar, $varchar, $varchar, $date, $date)
    """
      .command
      .contramap { case Note(id, title, content, author, _, _, _, visibility, publish, edit) =>
      id.value ~ title.value ~ content.value ~ author.value ~ visibility.toString ~ publish.value ~ edit.value}

  val updateNote: Command[Note] =
    sql"""
        update notes
        set note_title = $varchar,
            note_content = $varchar,
            note_visibility = $varchar
        where note_id = $noteId
    """.command.contramap { case Note(id, title, content, _, _, _, _, vis, _, _) =>
      title.value ~ content.value ~ vis.toString ~ id
    }

  val deleteNote: Query[Id, Note] =
    sql"""
        delete from notes where note_id = $noteId returning *
    """.query(decoder)

  val insertTag: Command[Tag] =
    sql"""
        insert into tag_map
        values ($tagEncoder)
    """.command
    
  val selectByUser: Query[String, Note] =
    sql"""
        select * from notes
        where note_author = $varchar
    """.query(decoder)

  val selectByTag: Query[TagName, Note] =
    sql"""
        select n.*
        from notes n, tag_map t
        where t.note_id = n.note_id
        and t.tag_id = $tagName
    """.query(decoder)

  val selectByTagAndUser: Query[TagName ~ String, Note] =
    sql"""
        select n.*
        from notes n, tag_map t
        where t.note_id = n.note_id
        and t.tag_id = $tagName
        and n.note_author = $varchar
    """.query(decoder)