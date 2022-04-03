package repositories

import cats.effect.{Concurrent, Resource}
import models.Tag.*
import models.Article.*
import Codecs.*
import skunk.*
import skunk.implicits.*
import skunk.codec.text.*
import skunk.codec.temporal.*
import cats.syntax.all.*
import skunk.codec.all.int8

import java.time.LocalDate

trait Blogs[F[_]]:
  def findAllBlogs: F[List[Article]]
  def findBlogById(id: Id): F[Option[Article]]
  def findBlogByUser(user: String): F[List[Article]]
  def findBlogByTag(tagName: TagName): F[List[Article]]
  def findBlogByTagAndUser(tagName: TagName, user: String): F[List[Article]]
  def create(blog: Article): F[Article]
  def update(blog: Article): F[Article]
  def delete(blogId: Id): F[Option[Article]]
  def addTag(tag: Tag): F[Tag]

object Blogs:
  import BlogsSql.*
  def make[F[_]: Concurrent](postgres: Resource[F, Resource[F, Session[F]]]): Blogs[F] =
    new Blogs[F] {
      override def findAllBlogs: F[List[Article]] = postgres.use(_.use(_.execute(selectAll)))

      override def findBlogById(id: Id): F[Option[Article]] = postgres.use(_.use { session =>
        session.prepare(selectById).use { ps =>
          ps.option(id)
        }
      })

      override def findBlogByUser(user: String): F[List[Article]] = postgres.use(_.use { session =>
        session.prepare(selectByUser).use { ps =>
          ps.stream(user, 15).compile.toList
        }
      })

      override def findBlogByTag(tagName: TagName): F[List[Article]] = postgres.use(_.use { session =>
        session.prepare(selectByTag).use { ps =>
          ps.stream(tagName, 15).compile.toList
        }
      })

      override def findBlogByTagAndUser(tagName: TagName, user: String): F[List[Article]] = postgres.use(_.use { session =>
        session.prepare(selectByTagAndUser).use { ps =>
          ps.stream((tagName, user),15).compile.toList
        }
      })

      override def create(blog: Article): F[Article] = postgres.use(_.use { session =>
        session.prepare(insertNote).use(_.execute(blog)).as(blog)
      })

      override def update(blog: Article): F[Article] = postgres.use(_.use { session =>
        session.prepare(updateNote).use(_.execute(blog)).as(blog)
      })

      override def delete(blogId: Id): F[Option[Article]] = postgres.use(_.use { session =>
        session.prepare(deleteNote).use(ps => ps.option(blogId))
      })

      override def addTag(tag: Tag): F[Tag] = postgres.use(_.use { session =>
        session.prepare(insertTag).use(_.execute(tag)).as(tag)
      })
    }

private object BlogsSql:
  val decoder: Decoder[Article] =
    ( blogId ~ varchar ~ varchar ~ blogAuthorId ~ int8 ~ varchar ~ date ~ date ).map {
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

  val encoder: Encoder[Article] =
    (varchar ~ varchar ~ varchar ~ varchar ~ varchar ~ date ~ date)
      .contramap { case Article(id, title, content, author, _, _, _, visibility, publish, edit) =>
      id.value ~ title.value ~ content.value ~ author.value ~ visibility.toString ~ publish.value ~ edit.value }

  val tagEncoder: Encoder[Tag] =
    ( varchar ~ varchar ~ varchar).contramap { t => t.id.value ~ t.name.value ~ t.blogId.value }

  val selectAll: Query[Void, Article] =
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

  val selectById: Query[Id, Article] =
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

  val insertNote: Command[Article] =
    sql"""
        insert into blogs
        values ($encoder)
    """.command

  val updateNote: Command[Article] =
    sql"""
        update blogs
        set blog_title = $varchar,
            blog_content = $varchar,
            blog_visibility = $varchar,
            blog_last_edit_date = $date
        where blog_id = $blogId
    """.command.contramap { case Article(id, title, content, _, _, _, _, vis, _, _) =>
      title.value ~ content.value ~ vis.toString ~ LocalDate.now ~ id
    }

  val deleteNote: Query[Id, Article] =
    sql"""
        delete from blogs where blog_id = $blogId returning *
    """.query(decoder)

  val insertTag: Command[Tag] =
    sql"""
        insert into tag_map
        values ($tagEncoder)
    """.command
    
  val selectByUser: Query[String, Article] =
    sql"""
        select * from blogs
        where blog_author = $varchar
    """.query(decoder)

  val selectByTag: Query[TagName, Article] =
    sql"""
        select n.*
        from blogs n, tag_map t
        where t.blog_id = n.blog_id
        and t.tag_id = $tagName
    """.query(decoder)

  val selectByTagAndUser: Query[TagName ~ String, Article] =
    sql"""
        select n.*
        from blogs n, tag_map t
        where t.blog_id = n.blog_id
        and t.tag_id = $tagName
        and n.blog_author = $varchar
    """.query(decoder)