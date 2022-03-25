package repositories


import cats.effect.{Resource, Concurrent, MonadCancelThrow}
import models.User.*
import Codecs.*
import skunk.*
import skunk.implicits.*
import skunk.codec.text.*
import skunk.codec.temporal.*
import cats.syntax.all.*

trait UsersSkunk[F[_]]:
  def findAllUsers: F[List[User]]
  def findUserById(id: UserId): F[Option[User]]
  def create(user: User): F[User]
  def update(user: User): F[User]
  def delete(userId: UserId): F[Option[User]]

object UsersSkunk:
  import UsersSql.*
  def make[F[_]: Concurrent](pg: Resource[F, Resource[F, Session[F]]]): UsersSkunk[F] =
    new UsersSkunk[F] {
      override def findAllUsers: F[List[User]] = pg.use(_.use(_.execute(selectAll)))

      override def findUserById(id: UserId): F[Option[User]] = pg.use(_.use { session =>
        session.prepare(selectById).use { ps =>
          ps.option(id)
        }
      })

      override def create(user: User): F[User] = pg.use(_.use { session =>
        session.prepare(insertUser).use(_.execute(user)).as(user)
      })

      override def update(user: User): F[User] = pg.use(_.use { session =>
        session.prepare(updateUser).use(_.execute(user)).as(user)
      })

      override def delete(userId: UserId): F[Option[User]] = pg.use(_.use { session =>
        session.prepare(deleteUser).use(ps => ps.option(userId))
      })
    }

private object UsersSql:
  import repositories.Codecs.*
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



  val selectAll: Query[Void, User] =
    sql"select * from authors".query(decoder)

  val selectById: Query[UserId, User] =
    sql"select * from authors where author_id = $userId".query(decoder)

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
        delete from authors where author_id = $userId returning *
    """.query(decoder)