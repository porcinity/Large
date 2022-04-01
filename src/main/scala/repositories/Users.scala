package repositories


import cats.effect.{Resource, Concurrent, MonadCancelThrow}
import models.User.*
import Codecs.*
import skunk.*
import skunk.implicits.*
import skunk.codec.text.*
import skunk.codec.temporal.*
import cats.syntax.all.*

trait Users[F[_]]:
  def findAllUsers: F[List[User]]
  def findUserById(id: UserId): F[Option[User]]
  def create(user: User): F[User]
  def update(user: User): F[User]
  def delete(userId: UserId): F[Option[User]]

object Users:
  import UsersSql.*
  def make[F[_]: Concurrent](pg: Resource[F, Resource[F, Session[F]]]): Users[F] =
    new Users[F] {
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
//  val decoder: Decoder[User] =
//    ( varchar ~ varchar ~ varchar ~ varchar ~ date ).map {
//      case idd ~ name ~ email ~ status ~ join =>
//        User(
//          idd,
//          name,
//          Email(
//            email,
//            EmailStatus.fromString(status)
//          ),
//          join
//        )
//    }

  val encoder: Encoder[User] =
    (
      varchar ~ varchar ~ varchar ~ varchar ~ date
    ).contramap { u =>
      u.id.value ~ u.name.value ~ u.email.address.value
        ~ EmailStatus.makeString(u.email.status) ~ u.joinDate.value }

  val codec: Codec[User] =
    (varchar ~ varchar ~ varchar ~ varchar ~ date).imap {
      case i ~ n ~ a ~ s ~ d => User(
        UserId.unsafeFrom(i),
        Username.unsafeFrom(n),
        Email(
          EmailAddress.unsafeFrom(a),
          EmailStatus.fromString(s)
        ),
        JoinDate(d)
      )
    } (u =>
      u.id.value ~ u.name.value ~ u.email.address.value ~
        EmailStatus.makeString(u.email.status) ~ u.joinDate.value)

  val selectAll: Query[Void, User] =
    sql"select * from users".query(codec)

  val selectById: Query[UserId, User] =
    sql"select * from users where user_id = $userId".query(codec)

  val insertUser: Command[User] =
    sql"""
        insert into users
        values ($codec)
        """.command

  val updateUser: Command[User] =
    sql"""
        update users
        set username = $varchar,
            user_email_address = $varchar,
            user_email_status = $varchar
        where user_id = $varchar
    """.command.contramap { case User(id, name, email, _) =>
      name.value ~ email.address.value ~ EmailStatus.makeString(email.status) ~ id.value}

  val deleteUser: Query[UserId, User] =
    sql"""
        delete from users where user_id = $userId returning *
    """.query(codec)