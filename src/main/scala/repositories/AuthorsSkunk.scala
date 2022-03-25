package repositories


import cats.effect.{Resource, Concurrent, MonadCancelThrow}
import models.User.*
import skunk.*
import skunk.implicits.*
import skunk.codec.text.*
import skunk.codec.temporal.*
import cats.syntax.all.*

trait AuthorsSkunk[F[_]]:
  def findAllAuthors: F[List[User]]
  def findAuthorById(id: UserId): F[Option[User]]
  def create(author: User): F[User]
  def update(author: User): F[User]
  def delete(authorId: UserId): F[Option[User]]

object AuthorsSkunk:
  import AuthorSql.*
  def make[F[_]: Concurrent](pg: Resource[F, Resource[F, Session[F]]]): AuthorsSkunk[F] =
    new AuthorsSkunk[F] {
      override def findAllAuthors: F[List[User]] = pg.use(_.use(_.execute(selectAll)))

      override def findAuthorById(id: UserId): F[Option[User]] = pg.use(_.use { session =>
        session.prepare(selectById).use { ps =>
          ps.option(id)
        }
      })

      override def create(author: User): F[User] = pg.use(_.use { session =>
        session.prepare(insertUser).use(_.execute(author)).as(author)
      })

      override def update(author: User): F[User] = pg.use(_.use { session =>
        session.prepare(updateUser).use(_.execute(author)).as(author)
      })

      override def delete(authorId: UserId): F[Option[User]] = pg.use(_.use { session =>
        session.prepare(deleteUser).use(ps => ps.option(authorId))
      })
    }

private object AuthorSql:
  val decoder: Decoder[User] =
    ( varchar ~ varchar ~ varchar ~ varchar ~ date ).map {
      case idd ~ name ~ email ~ status ~ join =>
        User(
          UserId(idd),
          Name(name),
          Email(
            EmailAddress(email),
            EmailStatus.fromString(status)
          ),
          JoinDate(join)
        )
    }

  val encoder: Encoder[User] =
    (
      varchar ~ varchar ~ varchar ~ varchar ~ date
    ).contramap { a =>
      a.id.value ~ a.name.value ~ a.email.address.value
        ~ EmailStatus.makeString(a.email.status) ~ a.joinDate.value }

  val codec: Codec[User] =
    (varchar ~ varchar ~ varchar ~ varchar ~ date).imap {
      case i ~ n ~ a ~ s ~ d => User(
        UserId(i),
        Name(n),
        Email(
          EmailAddress(a),
          EmailStatus.fromString(s)
        ),
        JoinDate(d)
      )
    } (a =>
      a.id.value ~ a.name.value ~ a.email.address.value ~
        EmailStatus.makeString(a.email.status) ~ a.joinDate.value)

  val authorId: Codec[UserId] =
    varchar.imap[UserId](UserId(_))(_.value)

  val selectAll: Query[Void, User] =
    sql"select * from authors".query(decoder)

  val selectById: Query[UserId, User] =
    sql"select * from authors where author_id = $authorId".query(decoder)

  val insertUser: Command[User] =
    sql"""
        insert into authors
        values ($codec)
        """.command

  val updateUser: Command[User] =
    sql"""
        update authors
        set author_name = $varchar,
            author_email = $varchar,
            author_email_status = $varchar
        where author_id = $varchar
    """.command.contramap { case User(id, name, email, _) =>
      name.value ~ email.address.value ~ EmailStatus.makeString(email.status) ~ id.value}

  val deleteUser: Query[UserId, User] =
    sql"""
        delete from authors where author_id = $authorId returning *
    """.query(decoder)