import cats.effect.{MonadCancelThrow, Resource}
import doobie.Transactor
import doobie.hikari.HikariTransactor
import doobie.implicits.*

case class Blog(id: Int, title: String, content: String)

trait Blogs[F[_]]:
  def findAll: F[List[Blog]]

object Blogs:
  def make[F[_]: MonadCancelThrow](postgres: Resource[F, Transactor[F]]): Blogs[F] =
    new Blogs[F] {
      override def findAll: F[List[Blog]] = postgres.use { xa =>
        sql"select post_id, post_title, post_content from junk".query[Blog].to[List].transact(xa)
      }
    }


