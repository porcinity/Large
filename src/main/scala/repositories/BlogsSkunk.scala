package repositories

import cats.effect.{Concurrent, Resource}
import models.Tag.*
import models.Blog.*
import skunk.*
import skunk.implicits.*
import skunk.codec.text.*
import skunk.codec.temporal.*

import cats.syntax.all.*

trait BlogsSkunk[F[_]]:
  def findAllBlogs: F[List[Blog]]
  def findBlogById(id: BlogId): F[Option[Blog]]
  def create(blog: Blog): F[Blog]
  def update(blog: Blog): F[Blog]
  def delete(blogId: BlogId): F[Option[Blog]]
  def addTag(tag: Tag): F[Tag]

object BlogsSkunk:
  import BlogsSql.*
  def make[F[_]: Concurrent](postgres: Resource[F, Resource[F, Session[F]]]): BlogsSkunk[F] =
    new BlogsSkunk[F] {
      override def findAllBlogs: F[List[Blog]] = postgres.use(_.use(_.execute(selectAll)))

      override def findBlogById(id: BlogId): F[Option[Blog]] = postgres.use(_.use { session =>
        session.prepare(selectById).use { ps =>
          ps.option(id)
        }
      })

      override def create(blog: Blog): F[Blog] = postgres.use(_.use { session =>
        session.prepare(insertBlog).use(_.execute(blog)).as(blog)
      })

      override def update(blog: Blog): F[Blog] = postgres.use(_.use { session =>
        session.prepare(updateBlog).use(_.execute(blog)).as(blog)
      })

      override def delete(blogId: BlogId): F[Option[Blog]] = postgres.use(_.use { session =>
        session.prepare(deleteBlog).use(ps => ps.option(blogId))
      })

      override def addTag(tag: Tag): F[Tag] = postgres.use(_.use { session =>
        session.prepare(insertTag).use(_.execute(tag)).as(tag)
      })
    }

private object BlogsSql:
  val authorId: Codec[BlogAuthor] =
    varchar.imap[BlogAuthor](BlogAuthor(_))(_.value)

  val blogId: Codec[BlogId] =
    varchar.imap[BlogId](BlogId(_))(_.value)

  val decoder: Decoder[Blog] =
    ( blogId ~ varchar ~ varchar ~ authorId).map {
      case bId ~ title ~ content ~ aId =>
        Blog(
          bId,
          BlogTitle(title),
          BlogContent(content),
          aId
        )
    }

  val encoder: Encoder[Blog] =
    (
      varchar ~ varchar ~ varchar ~ varchar
    ).contramap { case b =>
      b.id.value ~ b.title.titleVal ~ b.content.v ~ b.author.value
    }

  val tagEncoder: Encoder[Tag] =
    ( varchar ~ varchar ~ varchar).contramap { t => t.id.value ~ t.name.value ~ t.blogId.value }

  val selectAll: Query[Void, Blog] =
    sql"select * from junk".query(decoder)

  val selectById: Query[BlogId, Blog] =
    sql"select * from junk where post_id = $blogId".query(decoder)

  val insertBlog: Command[Blog] =
    sql"""
        insert into junk
        values ($encoder)
    """.command

  val updateBlog: Command[Blog] =
    sql"""
        update junk
        set post_title = $varchar,
            post_content = $varchar
        where post_id = $blogId
    """.command.contramap { case Blog(id, title, content, _) =>
      title.titleVal ~ content.v ~ id
    }
    
  val deleteBlog: Query[BlogId, Blog] =
    sql"""
        delete from junk where post_id = $blogId returning *
    """.query(decoder)

  val insertTag: Command[Tag] =
    sql"""
        insert into blog_tags
        values ($tagEncoder)
    """.command