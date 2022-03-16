package repo.BlogsRepo

import cats.effect.{IO, MonadCancelThrow, Resource}
import doobie.Transactor
import doobie.hikari.HikariTransactor
import doobie.implicits.*
import doobie.*
import doobie.postgres.*
import doobie.postgres.implicits.*
import io.circe.{Decoder, Encoder}
import io.circe.generic.auto.deriveEncoder
import io.circe.generic.semiauto.deriveDecoder
import org.http4s.EntityEncoder


case class Blog(id: Int, title: String, content: String)

opaque type BlogId = Int
object BlogId:
  def apply(id: Int): BlogId = id
  def fromInt(num: Int):BlogId = BlogId(num)
  def toInt(id: BlogId): Int = id
  implicit val natMeta: Meta[BlogId] = Meta[Int].imap(fromInt)(toInt)
//  implicit val entityEncoder: EntityEncoder[IO, BlogId] = EntityEncoder[IO, BlogId].contramap(x => BlogId(x))
  extension (x: BlogId)
    def value: Int = x
//  implicit val blogIdRead: Read[BlogId] =
//    Read[Int].map { case id => BlogId(id) }
//  implicit val blogIdGet: Get[BlogId] = Get[Int].map(BlogId(_))

opaque type Content = String
object Content:
  def apply(content: String): Content = content
  extension (x: Content)
    def value: String = x

opaque type Title = String
object Title:
  def apply(title: String): Title = title
  extension (x: Title)
    def value: String = x

case class RichBlog(id: BlogId, title: Title, content: Content)

implicit val richBlogRead: Read[RichBlog] =
  Read[(Int, String, String)].map { case (id, title, content) => RichBlog(BlogId(id), Title(title), Content(content)) }


import sprout.*
type TestSprout = TestSprout.Type
object TestSprout extends Sprout[String]
  with SproutShow[String]
  implicit val encoder: Encoder[TestSprout] = Encoder[TestSprout].contramap(x => x)
  implicit val entityEncoder: EntityEncoder[IO, TestSprout] = EntityEncoder[IO,TestSprout].contramap(x => x)
  implicit val decoder: Decoder[TestSprout] = Decoder[String].map(TestSprout(_))

implicit val sproutRead: Read[TestSprout] =
  Read[String].map(x => TestSprout(x))

trait Blogs[F[_]]:
  def findAll: F[List[Blog]]
  def findById(id: Int): F[Option[Blog]]
  def findRichIdById(id: Int): F[Option[BlogId]]
  def findAllRichIds: F[List[BlogId]]
  def findAllRichBlogs(): F[List[RichBlog]]
  def findSprout: F[List[TestSprout]]

object Blogs:
  def make[F[_]: MonadCancelThrow](postgres: Resource[F, Transactor[F]]): Blogs[F] =
    new Blogs[F] {
      override def findAll: F[List[Blog]] = postgres.use { xa =>
        sql"select post_id, post_title, post_content from junk".query[Blog].to[List].transact(xa)
      }

      override def findById(id: Int): F[Option[Blog]] = postgres.use { xa =>
        sql"select post_id, post_title, post_content from junk where post_id = $id".query[Blog].option.transact(xa)
      }

      override def findRichIdById(id: Int): F[Option[BlogId]] = postgres.use { xa =>
        sql"select post_id from junk where post_id = $id".query[BlogId].option.transact(xa)
      }

      override def findAllRichIds: F[List[BlogId]] = postgres.use { xa =>
        sql"select post_id from junk".query[BlogId].to[List].transact(xa)
      }

      override def findAllRichBlogs(): F[List[RichBlog]] =  postgres.use { xa =>
        sql"select post_id, post_title, post_content from junk".query[RichBlog].to[List].transact(xa)
      }

      override def findSprout: F[List[TestSprout]] = postgres.use { xa =>
        sql"select post_title from junk".query[TestSprout].to[List].transact(xa)
      }
    }


