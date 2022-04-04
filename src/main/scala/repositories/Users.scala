package repositories


import cats.effect.{Concurrent, MonadCancelThrow, Resource}
import models.User.*
import Codecs.*
import skunk.*
import skunk.implicits.*
import skunk.codec.text.*
import skunk.codec.numeric.*
import skunk.codec.temporal.*
import skunk.data.Arr
import cats.syntax.all.*
import models.User.MembershipTier.makeString

trait Users[F[_]]:
  def findAllUsers: F[List[User]]
  def findUserById(id: UserId): F[Option[User]]
  def create(user: User): F[User]
  def update(user: User): F[User]
  def delete(userId: UserId): F[Option[User]]
  def followUser(userId: UserId, followerId: UserId): F[Unit]
  def unfollowUser(userId: UserId, followerId: UserId): F[Unit]

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

      override def followUser(userId: UserId, followerId: UserId): F[Unit] = pg.use(_.use { session =>
        session.prepare(insertFollowMap).use(_.execute(userId, followerId)).void
      })

      override def unfollowUser(userId: UserId, followerId: UserId): F[Unit] = pg.use(_.use { session =>
        session.prepare(deleteFollowMap).use(_.execute(userId, followerId)).void
      })
    }

private object UsersSql:
  import repositories.Codecs.*

  val codec: Codec[User] =
    (varchar ~ varchar ~ varchar ~ varchar ~ varchar ~ varchar ~ int8 ~ int8 ~ int8 ~ date ~ _varchar).imap {
      case i ~ n ~ b ~ a ~ s ~ t ~ followers ~ following ~ l ~ d ~ list => User(
        UserId.unsafeFrom(i),
        Username.unsafeFrom(n),
        Biography.unsafeFrom(b),
        Email(
          EmailAddress.unsafeFrom(a),
          EmailStatus.fromString(s)
        ),
        MembershipTier.fromString(t),
        Followers.unsafeFrom(followers.toInt),
        Following.unsafeFrom(following.toInt),
        Liked.unsafeFrom(l.toInt),
        JoinDate(d),
        list.toList
      )
    } (u =>
      u.id.value ~ u.name.value ~ u.bio.value ~ u.email.address.value ~
        EmailStatus.makeString(u.email.status) ~ makeString(u.tier) ~ u.followers.value.toLong ~ u.following.value.toLong ~
        u.likedArticles.value.toLong ~ u.joinDate.value ~ Arr.fromFoldable(u.articles))

  val selectAll: Query[Void, User] =
   sql"""
    select u.user_id, u.username, u.user_bio, u.user_email_address, u.user_email_status, u.user_tier,
       (select count(*) from follows_map f where f.user_id = u.user_id) as followers,
       (select count(*) from follows_map f where f.follower_id = u.user_id) as following,
       (select count(*) from likes_map l where l.like_user = u.user_id) as likes,
       u.user_join_date,
       array_remove(array_agg(a.article_id), NULL) as articles
    from users u
    left join articles a on u.user_id = a.article_author
    group by u.user_id
    """.query(codec)

  val selectById: Query[UserId, User] =
    sql"""
        select u.user_id, u.username, u.user_bio, u.user_email_address, u.user_email_status, u.user_tier,
                (select count(*) from follows_map f where f.user_id = u.user_id),
                (select count(*) from follows_map f where f.follower_id = u.user_id),
                (select count(*) from likes_map l where l.like_user = u.user_id),
                u.user_join_date,
                array_remove(array_agg(a.article_id), NULL) as articles
         from users u
         left join articles a on u.user_id = a.article_author
         where u.user_id = $userId
         group by u.user_id
         """
      .query(codec)

  val insertUser: Command[User] =
    sql"""
        insert into users(user_id, username, user_email_address, user_email_status, user_tier, user_join_date, user_bio)
        values ($varchar, $varchar, $varchar, $varchar, $varchar, $date, $varchar)
        """
      .command
      .contramap { case User(id, name, bio, email, tier, _, _, _, j, _) =>
      id.value ~ name.value ~ email.address.value ~ EmailStatus.makeString(email.status) ~ makeString(tier) ~ j.value ~ bio.value}

  val updateUser: Command[User] =
    sql"""
        update users
        set username = $varchar,
            user_email_address = $varchar,
            user_email_status = $varchar
        where user_id = $varchar
    """.command.contramap { case User(id, name, _, email, _, _, _, _, _, _) =>
      name.value ~ email.address.value ~ EmailStatus.makeString(email.status) ~ id.value}

  val deleteUser: Query[UserId, User] =
    sql"""
        delete from users where user_id = $userId returning *
    """.query(codec)

  val insertFollowMap: Command[UserId ~ UserId] =
    sql"""
        insert into follows_map (user_id, follower_id)
        values ($userId, $userId)
         """.command

  val deleteFollowMap: Command[UserId ~ UserId] =
    sql"""
         delete from follows_map
         where user_id = $userId
         and follower_id = $userId
         """.command