package repositories

import cats.effect.{Concurrent, Resource, MonadCancelThrow}
import doobie.Transactor
import doobie.implicits.*
import doobie.postgres.*
import doobie.postgres.implicits.*
import models.User.*
import models.User.Codecs.{authorRead, authorWrite}
import cats.syntax.functor.*

trait Users[F[_]]:
  def findAllUsers: F[List[User]]
  def findUserById(id: String): F[Option[User]]
  def create(author: User): F[User]
  def update(author: User): F[User]
  def delete(id: String): F[Either[String, Int]]

object Users:
  def make[F[_]: MonadCancelThrow](postgres: Resource[F, Transactor[F]]): Users[F] =
    new Users[F] {
      override def findAllUsers: F[List[User]] = postgres.use { xa =>
        sql"select author_id, author_name, author_email, author_email_status, author_join_date from authors"
          .query[User].to[List].transact(xa)
      }

      override def findUserById(id: String): F[Option[User]] = postgres.use { xa =>
        sql"select author_id, author_name, author_email, author_email_status, author_join_date from authors where author_id = $id"
          .query[User].option.transact(xa)
      }

      override def create(author: User): F[User] = postgres.use { xa =>
        val id = author.id.value
        val name = author.name.value
        val email = author.email.address.value
        val emailStatus = EmailStatus.makeString(author.email.status)
        val joinDate = author.joinDate.value
        sql"""
          insert into authors
          (author_id, author_name, author_email, author_email_status, author_join_date)
          values ($id, $name, $email, $emailStatus, $joinDate)
        """
          .update
          .withUniqueGeneratedKeys("*")
          .transact(xa)
      }

      override def update(author: User): F[User] = postgres.use { xa =>
        val id = author.id.value
        val name = author.name.value
        val email = author.email.address.value
        val status = EmailStatus.makeString(author.email.status)
        sql"""
            update authors
            set author_name = $name,
                author_email = $email,
                author_email_status = $status
            where author_id = $id
        """
          .update
          .withUniqueGeneratedKeys("*")
          .transact(xa)
      }

      override def delete(id: String): F[Either[String, Int]] = postgres.use { xa =>
        sql"delete from authors where author_id = $id"
          .update.run.transact(xa).map { x =>
            if (x == 0) Left("Blog not found.")
            else Right(x)
        }
      }
    }