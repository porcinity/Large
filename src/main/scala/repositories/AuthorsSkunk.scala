package repositories


import cats.effect.{Resource, Concurrent, MonadCancelThrow}
import models.Author.*
import skunk.*
import skunk.implicits.*
import skunk.codec.text.*
import skunk.codec.temporal.*

trait AuthorsSkunk[F[_]]:
  def findAllAuthors: F[List[Author]]
  def findAuthorById(id: AuthorId): F[Option[Author]]

object AuthorsSkunk:
  import AuthorSql.*
  def make[F[_]: Concurrent](pg: Resource[F, Session[F]]): AuthorsSkunk[F] =
    new AuthorsSkunk[F] {
      override def findAllAuthors: F[List[Author]] = pg.use(_.execute(selectAll))
      
      override def findAuthorById(id: AuthorId): F[Option[Author]] = pg.use { session =>
        session.prepare(selectById).use { ps =>
          ps.option(id)
        }
      }
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

  val authorId: Codec[AuthorId] =
    varchar.imap[AuthorId](AuthorId(_))(_.value)

  val selectAll: Query[Void, Author] =
    sql"select * from authors".query(decoder)

  val selectById: Query[AuthorId, Author] =
    sql"select * from authors where author_id = $authorId".query(decoder)