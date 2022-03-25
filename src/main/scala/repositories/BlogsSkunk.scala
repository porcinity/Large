package repositories

import cats.effect.{Concurrent, Resource}
import models.Author.AuthorId
import models.Blog.*
import skunk.*
import skunk.implicits.*
import skunk.codec.text.*
import skunk.codec.temporal.*

trait BlogsSkunk[F[_]]:
  def findAllBlogs: F[List[Blog]]
  def findBlogById(id: BlogId): F[Option[Blog]]
  def create(blog: Blog): F[Blog]
  def update(blog: Blog): F[Blog]
  def delete(blogId: BlogId): F[Option[Blog]]

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

      override def create(blog: Blog): F[Blog] = ???

      override def update(blog: Blog): F[Blog] = ???

      override def delete(blogId: BlogId): F[Option[Blog]] = ???
    }

private object BlogsSql:
  val authorId: Codec[AuthorId] =
    varchar.imap[AuthorId](AuthorId(_))(_.value)

  val blogId: Codec[BlogId] =
    varchar.imap[BlogId](BlogId(_))(_.value)

  val decoder: Decoder[Blog] =
    ( varchar ~ varchar ~ varchar ~ varchar).map {
      case bId ~ title ~ content ~ aId =>
        Blog(
          BlogId(bId),
          BlogTitle(title),
          BlogContent(content),
          BlogAuthor(aId)
        )
    }

  val selectAll: Query[Void, Blog] =
    sql"select * from junk".query(decoder)
    
  val selectById: Query[BlogId, Blog] =
    sql"select * from junk where post_id = $blogId".query(decoder)