package repositories

import cats.effect.{Concurrent, Resource}
import models.Tag.*
import models.Blog.*
import Codecs.*
import skunk.*
import skunk.implicits.*
import skunk.codec.text.*
import skunk.codec.temporal.*
import cats.syntax.all.*
import skunk.codec.all.int8

import java.time.LocalDate

trait Notes[F[_]]:
  def findAllBlogs: F[List[Blog]]
  def findBlogById(id: Id): F[Option[Blog]]
  def findBlogByUser(user: String): F[List[Blog]]
  def findBlogByTag(tagName: TagName): F[List[Blog]]
  def findBlogByTagAndUser(tagName: TagName, user: String): F[List[Blog]]
  def create(note: Blog): F[Blog]
  def update(note: Blog): F[Blog]
  def delete(noteId: Id): F[Option[Blog]]
  def addTag(tag: Tag): F[Tag]

object Notes:
  import NotesSql.*
  def make[F[_]: Concurrent](postgres: Resource[F, Resource[F, Session[F]]]): Notes[F] =
    new Notes[F] {
      override def findAllBlogs: F[List[Blog]] = postgres.use(_.use(_.execute(selectAll)))

      override def findBlogById(id: Id): F[Option[Blog]] = postgres.use(_.use { session =>
        session.prepare(selectById).use { ps =>
          ps.option(id)
        }
      })

      override def findBlogByUser(user: String): F[List[Blog]] = postgres.use(_.use { session =>
        session.prepare(selectByUser).use { ps =>
          ps.stream(user, 15).compile.toList
        }
      })

      override def findBlogByTag(tagName: TagName): F[List[Blog]] = postgres.use(_.use { session =>
        session.prepare(selectByTag).use { ps =>
          ps.stream(tagName, 15).compile.toList
        }
      })

      override def findBlogByTagAndUser(tagName: TagName, user: String): F[List[Blog]] = postgres.use(_.use { session =>
        session.prepare(selectByTagAndUser).use { ps =>
          ps.stream((tagName, user),15).compile.toList
        }
      })

      override def create(note: Blog): F[Blog] = postgres.use(_.use { session =>
        session.prepare(insertNote).use(_.execute(note)).as(note)
      })

      override def update(note: Blog): F[Blog] = postgres.use(_.use { session =>
        session.prepare(updateNote).use(_.execute(note)).as(note)
      })

      override def delete(noteId: Id): F[Option[Blog]] = postgres.use(_.use { session =>
        session.prepare(deleteNote).use(ps => ps.option(noteId))
      })

      override def addTag(tag: Tag): F[Tag] = postgres.use(_.use { session =>
        session.prepare(insertTag).use(_.execute(tag)).as(tag)
      })
    }

private object NotesSql:
  val decoder: Decoder[Blog] =
    ( blogId ~ varchar ~ varchar ~ noteAuthorId ~ int8 ~ varchar ~ date ~ date ).map {
      case nId ~ title ~ content ~ aId ~ likes ~ vis ~ publish ~ edit =>
        Blog(
          nId,
          Title.unsafeFrom(title),
          Content.unsafeFrom(content),
          aId,
          WordCount.unsafeFrom(content.split(" ").length),
          ReadingTime.unsafeFrom(content.split(" ").length / 200.0),
          Likes.unsafeFrom(likes.toInt),
          Visibility.unsafeFromString(vis),
          BlogDate(publish),
          BlogDate(edit)
        )
    }

  val encoder: Encoder[Blog] =
    (varchar ~ varchar ~ varchar ~ varchar ~ varchar ~ date ~ date)
      .contramap { case Blog(id, title, content, author, _, _, _, visibility, publish, edit) =>
      id.value ~ title.value ~ content.value ~ author.value ~ visibility.toString ~ publish.value ~ edit.value }

  val tagEncoder: Encoder[Tag] =
    ( varchar ~ varchar ~ varchar).contramap { t => t.id.value ~ t.name.value ~ t.noteId.value }

  val selectAll: Query[Void, Blog] =
    sql"""
         select b.blog_id,
                b.blog_title,
                b.blog_content,
                b.blog_author,
                (select count(*) from likes_map l where l.like_blog = b.blog_id) as likes,
                b.blog_visibility,
                b.blog_publish_date,
                b.blog_last_edit_date
         from blogs b;
    """.query(decoder)

  val selectById: Query[Id, Blog] =
    sql"""
         select b.blog_id,
                b.blog_title,
                b.blog_content,
                b.blog_author,
                (select count(*) from likes_map l where l.like_blog = b.blog_id) as likes,
                b.blog_visibility,
                b.blog_publish_date,
                b.blog_last_edit_date
         from blogs b
        where b.blog_id = $blogId;
    """.query(decoder)

  val insertNote: Command[Blog] =
    sql"""
        insert into blogs
        values ($encoder)
    """.command

  val updateNote: Command[Blog] =
    sql"""
        update blogs
        set blog_title = $varchar,
            blog_content = $varchar,
            blog_visibility = $varchar,
            blog_last_edit_date = $date
        where blog_id = $blogId
    """.command.contramap { case Blog(id, title, content, _, _, _, _, vis, _, _) =>
      title.value ~ content.value ~ vis.toString ~ LocalDate.now ~ id
    }

  val deleteNote: Query[Id, Blog] =
    sql"""
        delete from blogs where blog_id = $blogId returning *
    """.query(decoder)

  val insertTag: Command[Tag] =
    sql"""
        insert into tag_map
        values ($tagEncoder)
    """.command
    
  val selectByUser: Query[String, Blog] =
    sql"""
        select * from blogs
        where blog_author = $varchar
    """.query(decoder)

  val selectByTag: Query[TagName, Blog] =
    sql"""
        select n.*
        from blogs n, tag_map t
        where t.blog_id = n.blog_id
        and t.tag_id = $tagName
    """.query(decoder)

  val selectByTagAndUser: Query[TagName ~ String, Blog] =
    sql"""
        select n.*
        from blogs n, tag_map t
        where t.blog_id = n.blog_id
        and t.tag_id = $tagName
        and n.blog_author = $varchar
    """.query(decoder)