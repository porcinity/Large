package repo.BlogsRepo

import cats.effect.{Concurrent, IO, MonadCancelThrow, Resource, Sync}
import doobie.Transactor
import doobie.hikari.HikariTransactor
import doobie.implicits.*
import doobie.*
import doobie.postgres.*
import doobie.postgres.implicits.*
import io.circe.{Codec, Decoder, Encoder}
import io.circe.generic.auto.deriveEncoder
import io.circe.generic.semiauto.{deriveCodec, deriveDecoder}
import models.KewlBlog.*
import org.http4s.EntityEncoder

// This is needed to use map on F in the repository methods
import cats.syntax.functor.*

trait Blogs[F[_]]:
  def findAllKewlBlogs: F[List[KewlBlog]]
  def findKewlBlogById(id: Int): F[Option[KewlBlog]]
  def create(id: Int, title: String, content: String): F[Int]
  def update(id: Int, title: String, content: String): F[Int]
  def delete(id: Int): F[Either[String, Int]]


object Blogs:
  def make[F[_]: Concurrent](postgres: Resource[F, Transactor[F]]): Blogs[F] =
    new Blogs[F] {
      override def findAllKewlBlogs: F[List[KewlBlog]] =  postgres.use { xa =>
        sql"select post_id, post_title, post_content from junk".query[KewlBlog].to[List].transact(xa)
      }

      override def findKewlBlogById(id: Int): F[Option[KewlBlog]] = postgres.use { xa =>
        sql"select post_id, post_title, post_content from junk where post_id = $id ".query[KewlBlog].option.transact(xa)
      }

      override def create(id: Int, title: String, content: String): F[Int] = postgres.use { xa =>
        sql"insert into junk (post_id, post_title, post_content) values ($id, $title, $content)".update.withUniqueGeneratedKeys[Int]("post_id").transact(xa)
      }

      override def update(id: Int, title: String, content: String): F[Int] = postgres.use { xa =>
        sql"update junk set post_title = $title, post_content = $content where post_id = $id".update.withUniqueGeneratedKeys[Int]("post_id").transact(xa)
      }

      override def delete(id: Int): F[Either[String, Int]] = postgres.use { xa =>
        sql"delete from junk where post_id = $id".update.run.transact(xa).map { x =>
          if (x == 0) Left("Blog not found.")
          else Right(x)
        }
      }

    }


