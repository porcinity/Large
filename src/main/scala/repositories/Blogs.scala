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
  def findAllBlogs: F[List[Blog]]
  def findBlogById(id: String): F[Option[Blog]]
  def create(id: String, title: String, content: String): F[Blog]
  def insertBlog(blog: Blog): F[Blog]
  def update(blog: Blog): F[Blog]
  def deleteBlog(id: String): F[Either[String, Int]]


object Blogs:
  def make[F[_]: Concurrent](postgres: Resource[F, Transactor[F]]): Blogs[F] =
    new Blogs[F] {
      override def findAllBlogs: F[List[Blog]] =  postgres.use { xa =>
        sql"select post_id, post_title, post_content, post_author from junk".query[Blog].to[List].transact(xa)
      }

      override def findBlogById(id: String): F[Option[Blog]] = postgres.use { xa =>
        sql"select post_id, post_title, post_content, post_author from junk where post_id = $id ".query[Blog].option.transact(xa)
      }

      override def create(id: String, title: String, content: String): F[Blog] = postgres.use { xa =>
        sql"insert into junk (post_id, post_title, post_content) values ($id, $title, $content)".update
          .withUniqueGeneratedKeys("*").transact(xa)
      }

      override def insertBlog(blog: Blog): F[Blog] = postgres.use { xa =>
        val id = blog.id.value
        val title = blog.title.titleVal
        val content = blog.content.v
        val author = blog.author.authorVal
        sql"insert into junk (post_id, post_title, post_content, post_author) values ($id, $title, $content, $author)"
            .update
            .withUniqueGeneratedKeys("*").transact(xa)
      }

      override def update(blog: Blog): F[Blog] = postgres.use { xa =>
        val id = blog.id.value
        val title = blog.title.titleVal
        val content = blog.content.v
        sql"update junk set post_title = $title, post_content = $content where post_id = $id".update
          .withUniqueGeneratedKeys("*").transact(xa)
      }

      override def deleteBlog(id: String): F[Either[String, Int]] = postgres.use { xa =>
        sql"delete from junk where post_id = $id".update.run.transact(xa).map { x =>
          if (x == 0) Left("Blog not found.")
          else Right(x)
        }
      }

    }


