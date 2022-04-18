package io.porcinity.large.persistence

import cats.effect.{Concurrent, Resource}
import io.porcinity.large.domain.Tag.*
import io.porcinity.large.domain.Article.*
import Codecs.*
import skunk.*
import skunk.implicits.*
import skunk.codec.text.*
import skunk.codec.temporal.*
import cats.syntax.all.*
import io.porcinity.large.domain.Article.Visibility.makeString
import skunk.codec.all.int8
import io.porcinity.large.domain.User.UserId

import java.time.LocalDate

trait Articles[F[_]]:
  def findAllArticles: F[List[Article]]
  def findArticleById(id: Id): F[Option[Article]]
  def findArticleByUser(user: String): F[List[Article]]
  def findArticleByTag(tagName: TagName): F[List[Article]]
  def findArticleByTagAndUser(tagName: TagName, user: String): F[List[Article]]
  def create(article: Article): F[Article]
  def update(article: Article): F[Article]
  def delete(articleId: Id): F[Option[Article]]
  def addTag(tag: Tag): F[Tag]
  def likeArticle(articleId: Id, userId: UserId): F[Unit]
  def unlikeArticle(articleId: Id, userId: UserId): F[Unit]

object Articles:
  import ArticlesSql.*
  def make[F[_]: Concurrent](postgres: Resource[F, Session[F]]): Articles[F] =
    new Articles[F] {
      override def findAllArticles: F[List[Article]] =
        postgres.use(_.execute(selectAll))

      override def findArticleById(id: Id): F[Option[Article]] = postgres.use {
        session =>
          session.prepare(selectById).use { ps =>
            ps.option(id)
          }
      }

      override def findArticleByUser(user: String): F[List[Article]] =
        postgres.use { session =>
          session.prepare(selectByUser).use { ps =>
            ps.stream(user, 15).compile.toList
          }
        }

      override def findArticleByTag(tagName: TagName): F[List[Article]] =
        postgres.use { session =>
          session.prepare(selectByTag).use { ps =>
            ps.stream(tagName, 15).compile.toList
          }
        }

      override def findArticleByTagAndUser(
          tagName: TagName,
          user: String
      ): F[List[Article]] = postgres.use { session =>
        session.prepare(selectByTagAndUser).use { ps =>
          ps.stream((tagName, user), 15).compile.toList
        }
      }

      override def create(article: Article): F[Article] = postgres.use {
        session =>
          session.prepare(insertArticle).use(_.execute(article)).as(article)
      }

      override def update(article: Article): F[Article] = postgres.use {
        session =>
          session.prepare(updateArticle).use(_.execute(article)).as(article)
      }

      override def delete(blogId: Id): F[Option[Article]] = postgres.use {
        session =>
          session.prepare(deleteArticle).use(ps => ps.option(blogId))
      }

      override def addTag(tag: Tag): F[Tag] = postgres.use { session =>
        session.prepare(insertTag).use(_.execute(tag)).as(tag)
      }

      override def likeArticle(articleId: Id, userId: UserId): F[Unit] =
        postgres.use { session =>
          session.prepare(insertLikeMap).use(_.execute(articleId, userId)).void
        }

      override def unlikeArticle(articleId: Id, userId: UserId): F[Unit] =
        postgres.use { session =>
          session.prepare(deleteLikeMap).use(_.execute(articleId, userId)).void
        }
    }

private object ArticlesSql:
  val decoder: Decoder[Article] =
    (articleId ~ varchar ~ varchar ~ articleAuthorId ~ int8 ~ varchar ~ date ~ date ~ _varchar)
      .map {
        case nId ~ title ~ content ~ aId ~ likes ~ vis ~ publish ~ edit ~ tags =>
          Article(
            nId,
            Title.unsafeFrom(title),
            Content.unsafeFrom(content),
            aId,
            WordCount.unsafeFrom(content.split(" ").length),
            ReadingTime.unsafeFrom(content.split(" ").length / 200.0),
            Likes.unsafeFrom(likes.toInt),
            Visibility.unsafeFromString(vis),
            ArticleDate(publish),
            ArticleDate(edit),
            tags.toList
          )
      }

  val encoder: Encoder[Article] =
    (varchar ~ varchar ~ varchar ~ varchar ~ varchar ~ date ~ date)
      .contramap {
        case Article(
              id,
              title,
              content,
              author,
              _,
              _,
              _,
              visibility,
              publish,
              edit,
              _
            ) =>
          id.value ~ title.value ~ content.value ~ author.value ~ makeString(
            visibility
          ) ~ publish.value ~ edit.value
      }

  val tagEncoder: Encoder[Tag] =
    (varchar ~ varchar ~ varchar).contramap { t =>
      t.id.value ~ t.name.value ~ t.articleId.value
    }

  val selectAll: Query[Void, Article] =
    sql"""
         select a.article_id,
                a.article_title,
                a.article_content,
                a.article_author,
                (select count(*) from likes_map l where l.like_article = a.article_id) as likes,
                a.article_visibility,
                a.article_publish_date,
                a.article_last_edit_date,
                array_remove(array_agg(tm.tag_id), NULL) as tags
         from articles a
         left join tag_map tm on a.article_id = tm.article_id
        group by a.article_id;
    """.query(decoder)

  val selectById: Query[Id, Article] =
    sql"""
         select a.article_id,
                a.article_title,
                a.article_content,
                a.article_author,
                (select count(*) from likes_map l where l.like_article = a.article_id) as likes,
                a.article_visibility,
                a.article_publish_date,
                a.article_last_edit_date,
                array_remove(array_agg(tm.tag_id), NULL) as tags
         from articles a
         left join tag_map tm on a.article_id = tm.article_id
        where a.article_id = $articleId
        group by a.article_id, a.article_title, a.article_content, a.article_author, a.article_visibility, a.article_publish_date, a.article_last_edit_date;
    """.query(decoder)

  val insertArticle: Command[Article] =
    sql"""
        insert into articles
        values ($encoder)
    """.command

  val updateArticle: Command[Article] =
    sql"""
        update articles
        set article_title = $varchar,
            article_content = $varchar,
            article_visibility = $varchar,
            article_last_edit_date = $date
        where article_id = $articleId
    """.command.contramap {
      case Article(id, title, content, _, _, _, _, vis, _, _, _) =>
        title.value ~ content.value ~ vis.toString ~ LocalDate.now ~ id
    }

  val deleteArticle: Query[Id, Article] =
    sql"""
        delete from articles where article_id = $articleId returning *
    """.query(decoder)

  val insertTag: Command[Tag] =
    sql"""
        insert into tag_map
        values ($tagEncoder)
    """.command

  val selectByUser: Query[String, Article] =
    sql"""
        select * from articles
        where article_author = $varchar
    """.query(decoder)

  val selectByTag: Query[TagName, Article] =
    sql"""
        select a.*
        from articles a, tag_map t
        where t.article_id = a.article_id
        and t.tag_id = $tagName
    """.query(decoder)

  val selectByTagAndUser: Query[TagName ~ String, Article] =
    sql"""
        select a.*
        from articles a, tag_map t
        where t.article_id = a.article_id
        and t.tag_id = $tagName
        and a.article_id = $varchar
    """.query(decoder)

  val insertLikeMap: Command[Id ~ UserId] =
    sql"""
        insert into likes_map (like_article, like_user)
        values ($articleId, $userId)
         """.command

  val deleteLikeMap: Command[Id ~ UserId] =
    sql"""
        delete from likes_map
        where like_article = $articleId
        and like_user = $userId
         """.command
