package io.porcinity.large.persistence

import cats.effect.{Concurrent, Resource}
import io.porcinity.large.domain.Tag.{Tag, TagName}
import Codecs.tagName
import skunk.codec.text.varchar
import skunk.{Codec, Command, Decoder, Query, Session, Void}
import skunk.implicits.*
import cats.implicits.*

trait Tags[F[_]]:
  def findAllTags: F[List[TagName]]
  def createTag(name: String): F[Unit]
//  def updateTag(name: String): F[Unit]
//  def delete(tag: Tag): F[Unit]

object Tags:
  import TagsSql.*
  def make[F[_]: Concurrent](postgres: Resource[F, Session[F]]): Tags[F] =
    new Tags[F] {
      override def findAllTags: F[List[TagName]] =
        postgres.use(_.execute(selectAll))

      override def createTag(name: String): F[Unit] = postgres.use { session =>
        session.prepare(insert).use(_.execute(TagName.unsafeFrom(name))).void
      }

//      override def updateTag(name: String): F[Unit] = ???
//
//      override def delete(tag: Tag): F[Unit] = ???
    }

private object TagsSql:
  val selectAll: Query[Void, TagName] =
    sql"select * from tags".query(tagName)

  val insert: Command[TagName] =
    sql"insert into tags values ($tagName)".command

//  val updateTag: Command[TagName] =
//    sql"update tags set tag_name = $codec where tag_name = $varchar".command
