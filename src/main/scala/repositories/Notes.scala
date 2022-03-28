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
  def findAllBlogs: F[List[Note]]
  def findBlogById(id: NoteId): F[Option[Note]]
  def findByUser(user: String): F[List[Note]]
  def findByTag(tagName: TagName): F[List[Note]]
  def findByTagAndUser(tagName: TagName, user: String): F[List[Note]]
  def create(blog: Note): F[Note]
  def update(blog: Note): F[Note]
  def delete(blogId: NoteId): F[Option[Note]]
  def addTag(tag: Tag): F[Tag]

object Notes:
  import NotesSql.*
  def make[F[_]: Concurrent](postgres: Resource[F, Resource[F, Session[F]]]): Notes[F] =
    new Notes[F] {
      override def findAllBlogs: F[List[Note]] = postgres.use(_.use(_.execute(selectAll)))

      override def findBlogById(id: NoteId): F[Option[Note]] = postgres.use(_.use { session =>
        session.prepare(selectById).use { ps =>
          ps.option(id)
        }
      })

      override def findByUser(user: String): F[List[Note]] = postgres.use(_.use { session =>
        session.prepare(selectByUser).use { ps =>
          ps.stream(user, 15).compile.toList
        }
      })

      override def findByTag(tagName: TagName): F[List[Note]] = postgres.use(_.use { session =>
        session.prepare(selectByTag).use { ps =>
          ps.stream(tagName, 15).compile.toList
        }
      })

      override def findByTagAndUser(tagName: TagName, user: String): F[List[Note]] = postgres.use(_.use { session =>
        session.prepare(selectByTagAndUser).use { ps =>
          ps.stream((tagName, user),15).compile.toList
        }
      })

      override def create(blog: Note): F[Note] = postgres.use(_.use { session =>
        session.prepare(insertBlog).use(_.execute(blog)).as(blog)
      })

      override def update(blog: Note): F[Note] = postgres.use(_.use { session =>
        session.prepare(updateBlog).use(_.execute(blog)).as(blog)
      })

      override def delete(blogId: NoteId): F[Option[Note]] = postgres.use(_.use { session =>
        session.prepare(deleteBlog).use(ps => ps.option(blogId))
      })

      override def addTag(tag: Tag): F[Tag] = postgres.use(_.use { session =>
        session.prepare(insertTag).use(_.execute(tag)).as(tag)
      })
    }

private object NotesSql:
  val decoder: Decoder[Note] =
    ( blogId ~ varchar ~ varchar ~ blogAuthorId).map {
      case bId ~ title ~ content ~ aId =>
        Note(
          bId,
          NoteTitle(title),
          NoteContent(content),
          aId
        )
    }

  val encoder: Encoder[Note] =
    (
      varchar ~ varchar ~ varchar ~ varchar
    ).contramap { case b =>
      b.id.value ~ b.title.titleVal ~ b.content.v ~ b.author.value
    }

  val tagEncoder: Encoder[Tag] =
    ( varchar ~ varchar ~ varchar).contramap { t => t.id.value ~ t.name.value ~ t.blogId.value }

  val selectAll: Query[Void, Note] =
    sql"select * from notes".query(decoder)

  val selectById: Query[NoteId, Note] =
    sql"select * from notes where note_id = $blogId".query(decoder)

  val insertBlog: Command[Note] =
    sql"""
        insert into notes
        values ($encoder)
    """.command

  val updateBlog: Command[Note] =
    sql"""
        update notes
        set note_title = $varchar,
            note_content = $varchar
        where note_id = $blogId
    """.command.contramap { case Note(id, title, content, _) =>
      title.titleVal ~ content.v ~ id
    }
    
  val deleteBlog: Query[NoteId, Note] =
    sql"""
        delete from notes where note_id = $blogId returning *
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