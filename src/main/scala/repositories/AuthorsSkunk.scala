package repositories


import cats.effect.{Resource, Concurrent, MonadCancelThrow}
import models.Author.*
import skunk.*
import skunk.implicits.*
import skunk.codec.text.*
import skunk.codec.temporal.*
import cats.syntax.all.*

trait AuthorsSkunk[F[_]]:
  def findAllAuthors: F[List[Author]]
  def findAuthorById(id: AuthorId): F[Option[Author]]
  def create(author: Author): F[Author]
  def update(author: Author): F[Author]
  def delete(authorId: AuthorId): F[Unit]

object AuthorsSkunk:
  import AuthorSql.*
  def make[F[_]: Concurrent](pg: Resource[F, Resource[F, Session[F]]]): AuthorsSkunk[F] =
    new AuthorsSkunk[F] {
      override def findAllAuthors: F[List[Author]] = pg.use(_.use(_.execute(selectAll)))

      override def findAuthorById(id: AuthorId): F[Option[Author]] = pg.use(_.use { session =>
        session.prepare(selectById).use { ps =>
          ps.option(id)
        }
      })

      override def create(author: Author): F[Author] = pg.use(_.use { session =>
        session.prepare(insertUser).use(_.execute(author)).as(author)
      })

      override def update(author: Author): F[Author] = pg.use(_.use { session =>
        session.prepare(updateUser).use(_.execute(author)).as(author)
      })

      override def delete(authorId: AuthorId): F[Unit] = pg.use(_.use { session =>
        session.prepare(deleteUser).use(_.execute(authorId)).void
    })
    }

private object AuthorSql:
  val decoder: Decoder[Author] =
    ( varchar ~ varchar ~ varchar ~ varchar ~ date ).map {
      case idd ~ name ~ email ~ status ~ join =>
        Author(
          AuthorId(idd),
          Name(name),
          Email(
            EmailAddress(email),
            EmailStatus.fromString(status)
          ),
          JoinDate(join)
        )
    }

  val encoder: Encoder[Author] =
    (
      varchar ~ varchar ~ varchar ~ varchar ~ date
    ).contramap { case a =>
      a.id.value ~ a.name.value ~ a.email.address.value ~ EmailStatus.makeString(a.email.status) ~ a.joinDate.value
    }

  val codec: Codec[Author] =
    (varchar ~ varchar ~ varchar ~ varchar ~ date).imap {
      case i ~ n ~ a ~ s ~ d => Author(
        AuthorId(i),
        Name(n),
        Email(
          EmailAddress(a),
          EmailStatus.fromString(s)
        ),
        JoinDate(d)
      )
    } (a =>
      a.id.value ~ a.name.value ~ a.email.address.value ~ EmailStatus.makeString(a.email.status) ~ a.joinDate.value)

  val authorId: Codec[AuthorId] =
    varchar.imap[AuthorId](AuthorId(_))(_.value)

  val selectAll: Query[Void, Author] =
    sql"select * from authors".query(decoder)

  val selectById: Query[AuthorId, Author] =
    sql"select * from authors where author_id = $authorId".query(decoder)

  val insertUser: Command[Author] =
    sql"""
        insert into authors
        values ($codec)
        """.command

  val updateUser: Command[Author] =
    sql"""
        update authors
        set author_name = $varchar,
            author_email = $varchar,
            author_email_status = $varchar
        where author_id = $varchar
    """.command.contramap { case Author(id, name, email, _) =>
      name.value ~ email.address.value ~ EmailStatus.makeString(email.status) ~ id.value}

  val deleteUser: Command[AuthorId] =
    sql"""
        delete from authors where author_id = $varchar
    """.command.contramap{ case a => a.value }