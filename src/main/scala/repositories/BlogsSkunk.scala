package repositories

import cats.effect.{Concurrent, Resource}
import models.Blog.*
import skunk.*
import skunk.implicits.*
import skunk.codec.text.*
import skunk.codec.temporal.*

trait BlogsSkunk[F[_]]:
  def findAllBlogs: F[List[Blog]]
  def findBlogById(id: BlogId): F[Option[Blog]]
  def create(blog: Blog): F[Blog]
  def update(blog: Blog): F[Blog]
  def delete(blogId: BlogId): F[Option[Blog]]

object BlogsSkunk:
  def make[F[_]: Concurrent](postgres: Resource[F, Resource[F, Session[F]]]): BlogsSkunk[F] =
    new BlogsSkunk[F] {
      override def findAllBlogs: F[List[Blog]] = ???

      override def findBlogById(id: BlogId): F[Option[Blog]] = ???

      override def create(blog: Blog): F[Blog] = ???

      override def update(blog: Blog): F[Blog] = ???

      override def delete(blogId: BlogId): F[Option[Blog]] = ???
    }
