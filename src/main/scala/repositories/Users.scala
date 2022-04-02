package repositories


import cats.effect.{Resource, Concurrent, MonadCancelThrow}
import models.User.*
import Codecs.*
import skunk.*
import skunk.implicits.*
import skunk.codec.text.*
import skunk.codec.numeric.*
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

  val codec: Codec[User] =
    (varchar ~ varchar ~ varchar ~ varchar ~ varchar ~ varchar ~ int4 ~ int4 ~ int4 ~ date).imap {
      case i ~ n ~ b ~ a ~ s ~ t ~ followers ~ following ~ l ~ d => User(
        UserId.unsafeFrom(i),
        Username.unsafeFrom(n),
        Biography.unsafeFrom(b),
        Email(
          EmailAddress.unsafeFrom(a),
          EmailStatus.fromString(s)
        ),
        MembershipTier.fromString(t),
        Followers.unsafeFrom(followers),
        Following.unsafeFrom(following),
        Liked.unsafeFrom(l),
        JoinDate(d)
      )
    } (u =>
      u.id.value ~ u.name.value ~ u.bio.value ~ u.email.address.value ~
        EmailStatus.makeString(u.email.status) ~ u.tier.toString ~ u.followers.value ~ u.following.value ~
        u.likedArticles.value ~ u.joinDate.value)

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
    """.command.contramap { case User(id, name, _, email, _, _, _, _, _) =>
      name.value ~ email.address.value ~ EmailStatus.makeString(email.status) ~ id.value}

  val deleteUser: Query[UserId, User] =
    sql"""
        delete from users where user_id = $userId returning *
    """.query(codec)