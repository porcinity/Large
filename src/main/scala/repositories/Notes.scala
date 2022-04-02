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
    ( noteId ~ varchar ~ varchar ~ noteAuthorId).map {
      case nId ~ title ~ content ~ aId =>
        Note(
          nId,
          Title.unsafeFrom(title),
          Content.unsafeFrom(content),
          aId
        )
    }

  val encoder: Encoder[Note] =
    (
      varchar ~ varchar ~ varchar ~ varchar
    ).contramap { case b =>
      b.id.value ~ b.title.value ~ b.content.value ~ b.author.value
    }

  val tagEncoder: Encoder[Tag] =
    ( varchar ~ varchar ~ varchar).contramap { t => t.id.value ~ t.name.value ~ t.noteId.value }

  val selectAll: Query[Void, Note] =
    sql"select * from notes".query(decoder)

  val selectById: Query[Id, Note] =
    sql"select * from notes where note_id = $noteId".query(decoder)

  val insertNote: Command[Note] =
    sql"""
        insert into notes
        values ($encoder)
    """.command

  val updateNote: Command[Note] =
    sql"""
        update notes
        set note_title = $varchar,
            note_content = $varchar
        where note_id = $noteId
    """.command.contramap { case Note(id, title, content, _) =>
      title.value ~ content.value ~ id
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