package routes

import cats.data.Validated.{Invalid, Valid}
import cats.effect.Concurrent
import cats.Monad
import common.*
import models.Article.*
import models.Tag.{TagDto, TagName}
import org.http4s.*
import org.http4s.Status.{Created, NoContent, NotFound, Ok, UnprocessableEntity}
import org.http4s.circe.*
import org.http4s.circe.CirceEntityCodec.circeEntityEncoder
import org.http4s.dsl.Http4sDsl
import org.http4s.implicits.*
import org.http4s.syntax.*
import repositories.Articles

// These are necessary to use for-comprehensions on F
import cats.syntax.flatMap.*
import cats.syntax.functor.*
//import cats.syntax.*
import cats.implicits.*


//import monocle.syntax.*
import monocle.Lens
import monocle.macros.GenLens
import monocle.macros.syntax.AppliedFocusSyntax
import monocle.syntax.all.*

// The type constraint of Concurrent is necessary to decode Json
class ArticlesRoutes[F[_]: JsonDecoder: Monad](repository: Articles[F]) extends Http4sDsl[F] {

  implicit val tagCoder: QueryParamDecoder[TagName] =
    QueryParamDecoder[String].map(TagName.unsafeFrom)

  object ArticleIdVar:
    def unapply(str: String): Option[Id] = Some(Id.unsafeFrom(str))

  object OptionalTagQueryParamMatcher extends  OptionalQueryParamDecoderMatcher[TagName]("tag")
  object OptionalUserIdParamMatch extends OptionalQueryParamDecoderMatcher[String]("user")

  val routes: HttpRoutes[F] = HttpRoutes.of[F] {

    case GET -> Root :? OptionalTagQueryParamMatcher(tag) +& OptionalUserIdParamMatch(user) => (tag, user) match {
      case (Some(t), Some(u)) => Ok(repository.findArticleByTagAndUser(t, u))
      case (Some(t), None) => Ok(repository.findArticleByTag(t))
      case (None, Some(u)) => Ok(repository.findArticleByUser(u))
      case (None, None) => Ok(repository.findAllArticles.map(GetItems.apply))
    }

    case GET -> Root / ArticleIdVar(id) =>
      for {
        article <- repository.findArticleById(id)
        res <- article.fold(NotFound())(n => Ok(GetItem(n)))
      } yield res

    case req @ POST -> Root =>
      for
        dto <- req.asJsonDecode[ArticleDto]
        article <- ArticleDto.toDomain(dto).pure[F]
        res <- article.fold(UnprocessableEntity(_), b =>
          Created(repository.create(b).map(GetItem.apply)))
      yield res

    case req @ POST -> Root / ArticleIdVar(id) / "addTag" =>
      for {
        dto <- req.asJsonDecode[TagDto]
        tag <- TagDto.toDomain(dto, id).pure[F]
        res <- tag.fold(UnprocessableEntity(_), t => Ok(repository.addTag(t)))
      } yield res

    case req @ PUT -> Root / ArticleIdVar(id) =>
      for {
        dto <- req.asJsonDecode[ArticleDto]
        foundArticle <- repository.findArticleById(id)
        updateArticle = ArticleDto.toDomain(dto)
        res <- (foundArticle, updateArticle) match
          case (None, _) => NotFound()
          case (_, Left(e)) => UnprocessableEntity(e)
          case (Some(b), Right(u)) =>
            val newArticle = b.copy(title = u.title, content = u.content, visibility = u.visibility)
            Created(repository.update(newArticle).map(GetItem.apply))
      } yield res

    case DELETE -> Root / ArticleIdVar(id) =>
      for {
        res <- repository.delete(id)
        y <- res.fold(NotFound())( _ => NoContent())
      } yield y
  }
}