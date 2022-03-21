package repositories

import cats.effect.{Concurrent, Resource}
import doobie.Transactor
import doobie.implicits.*
import doobie.*
import doobie.postgres.*
import doobie.postgres.implicits.*
import models.Blog.*
import cats.Apply

// This is needed to use map on F in the repository methods
import cats.syntax.functor.*

trait Blogs[F[_]]:
  def findAllKewlBlogs: F[List[Blog]]
  def findKewlBlogById(id: Int): F[Option[Blog]]
  def create(id: Int, title: String, content: String): F[Blog]
  def insertKewB(kewlBlog: Blog): F[Blog]
  def update(kewlBlog: Blog): F[Blog]
  def deleteBlog(id: Int): F[Either[String, Int]]


object Blogs:
  def make[F[_]: Concurrent](postgres: Resource[F, Transactor[F]]): Blogs[F] =
    new Blogs[F] {
      override def findAllKewlBlogs: F[List[Blog]] =  postgres.use { xa =>
        sql"select post_id, post_title, post_content from junk".query[Blog].to[List].transact(xa)
      }

      override def findKewlBlogById(id: Int): F[Option[Blog]] = postgres.use { xa =>
        sql"select post_id, post_title, post_content from junk where post_id = $id ".query[Blog].option.transact(xa)
      }

      override def create(id: Int, title: String, content: String): F[Blog] = postgres.use { xa =>
        sql"insert into junk (post_id, post_title, post_content) values ($id, $title, $content)".update
          .withUniqueGeneratedKeys("post_id", "post_title", "post_content").transact(xa)
      }

      override def insertKewB(kewlBlog: Blog): F[Blog] = postgres.use { xa =>
        val id = kewlBlog.id.value
        val title = kewlBlog.title.value
        val content = kewlBlog.content.v
        sql"insert into junk (post_id, post_title, post_content) values ($id, $title, $content)"
            .update
            .withUniqueGeneratedKeys("post_id", "post_title", "post_content").transact(xa)
      }

      override def update(kewlBlog: Blog): F[Blog] = postgres.use { xa =>
        val id = kewlBlog.id.value
        val title = kewlBlog.title.value
        val content = kewlBlog.content.v
        sql"update junk set post_title = $title, post_content = $content where post_id = $id".update
          .withUniqueGeneratedKeys("post_id", "post_title", "post_content").transact(xa)
      }

      override def deleteBlog(id: Int): F[Either[String, Int]] = postgres.use { xa =>
        sql"delete from junk where post_id = $id".update.run.transact(xa).map { x =>
          if (x == 0) Left("Blog not found.")
          else Right(x)
        }
      }

    }


