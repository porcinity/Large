package repo.BlogsRepo

import cats.effect.{IO, MonadCancelThrow, Resource}
import doobie.Transactor
import doobie.hikari.HikariTransactor
import doobie.implicits.*
import doobie.*
import doobie.postgres.*
import doobie.postgres.implicits.*
import io.circe.{Codec, Decoder, Encoder}
import io.circe.generic.auto.deriveEncoder
import io.circe.generic.semiauto.{deriveCodec, deriveDecoder}
import models.KewlBlog._
import org.http4s.EntityEncoder

trait Blogs[F[_]]:
  def findAllKewlBlogs: F[List[KewlBlog]]
  def findKewlBlogById(id: Int): F[Option[KewlBlog]]

object Blogs:
  def make[F[_]: MonadCancelThrow](postgres: Resource[F, Transactor[F]]): Blogs[F] =
    new Blogs[F] {
      override def findAllKewlBlogs: F[List[KewlBlog]] =  postgres.use { xa =>
        sql"select post_id, post_title, post_content from junk".query[KewlBlog].to[List].transact(xa)
      }

      override def findKewlBlogById(id: Int): F[Option[KewlBlog]] = postgres.use { xa =>
        sql"select post_id, post_title, post_content from junk where post_id = $id ".query[KewlBlog].option.transact(xa)
      }
    }


