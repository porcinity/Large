package repositories


import cats.effect.{Resource, Concurrent}
import models.Author.*
import skunk.*
import skunk.implicits.*
import skunk.codec.text.*
import skunk.codec.temporal.*

trait AuthorsSkunk[F[_]]:
  def findAllAuthorsSkunk: F[List[Author]]

object AuthorsSkunk:
  import AuthorSql.*
  def make[F[_]: Concurrent](pg: Resource[F, Session[F]]): AuthorsSkunk[F] =
    new AuthorsSkunk[F] {
      override def findAllAuthorsSkunk: F[List[Author]] = pg.use(_.execute(selectAll))
    }

private object AuthorSql:
  val decoder: Decoder[Author] =
    ( varchar ~ varchar ~ varchar ~ varchar ~ date ).map {
      case id ~ name ~ email ~ status ~ join =>
        Author(
          AuthorId(id),
          Name(name),
          Email(
            EmailAddress(email),
            EmailStatus.fromString(status)
          ),
          JoinDate(join)
        )
    }

  val selectAll: Query[Void, Author] =
    sql"select * from authors".query(decoder)