package repositories

import cats.effect.{Concurrent, Resource}
import doobie.Transactor
import doobie.implicits.*
import doobie.postgres.*
import doobie.postgres.implicits.*
import models.Author.*

trait Authors[F[_]]:
  def findAllAuthors: F[List[Author]]
//  def findAuthorById(id: Int): F[Option[Author]]
//  def create(id: Int, title: String, content: String): F[Int]
//  def update(id: Int, title: String, content: String): F[Int]
//  def delete(id: Int): F[Either[String, Int]]

object Authors:
  def make[F[_]: Concurrent](postgres: Resource[F, Transactor[F]]): Authors[F] =
    new Authors[F] {
      override def findAllAuthors: F[List[Author]] = postgres.use { xa =>
        sql"select author_id, author_name, author_email, author_emailStatus from authors".query[Author].to[List].transact(xa)
      }

//      override def findAuthorById(id: Int): F[Option[Author]] = ???
//
//      override def create(id: Int, title: String, content: String): F[Int] = ???
//
//      override def update(id: Int, title: String, content: String): F[Int] = ???
//
//      override def delete(id: Int): F[Either[String, Int]] = ???
    }