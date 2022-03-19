package repositories

import cats.effect.{Concurrent, Resource}
import doobie.Transactor
import doobie.implicits.*
import doobie.postgres.*
import doobie.postgres.implicits.*
import models.Author.*
import cats.syntax.functor.*

trait Authors[F[_]]:
  def findAllAuthors: F[List[Author]]
  def findAuthorById(id: Int): F[Option[Author]]
  def create(author: Author): F[Author]
  def update(author: Author): F[Author]
  def delete(id: Int): F[Either[String, Int]]

object Authors:
  def make[F[_]: Concurrent](postgres: Resource[F, Transactor[F]]): Authors[F] =
    new Authors[F] {
      override def findAllAuthors: F[List[Author]] = postgres.use { xa =>
        sql"select author_id, author_name, author_email, author_email_status from authors"
          .query[Author].to[List].transact(xa)
      }

      override def findAuthorById(id: Int): F[Option[Author]] = postgres.use { xa =>
        sql"select author_id, author_name, author_email, author_email_status from authors where author_id = $id"
          .query[Author].option.transact(xa)
      }

      override def create(author: Author): F[Author] = postgres.use { xa =>
        val id = author.id.value
        val name = author.name.value
        val email = author.email.address.value
        val emailStatus = author.email.status.value
        sql"insert into authors (author_id, author_name, author_email, author_email_status) values ($id, $name, $email, $emailStatus)"
          .update.withUniqueGeneratedKeys("author_id", "author_name", "author_email", "author_email_status").transact(xa)
      }

      override def update(author: Author): F[Author] = postgres.use { xa =>
        val id = author.id.value
        val name = author.name.value
        val email = author.email.address.value
        sql"update authors set author_name = $name, author_email = $email where author_id = $id"
          .update.withUniqueGeneratedKeys("author_id", "author_name", "author_email", "author_email_status").transact(xa)
      }
//
      override def delete(id: Int): F[Either[String, Int]] = postgres.use { xa =>
        sql"delete from authors where author_id = $id"
          .update.run.transact(xa).map { x =>
            if (x == 0) Left("Blog not found.")
            else Right(x)
        }
      }
    }
    
    
// Come back to this
// map { x =>
//          case Author(x) => Some(x)
//          case _ => None
//        }