package routes

import cats.data.Validated.{Invalid, Valid}
import cats.effect.Concurrent
import models.Blog.*
import models.Tag.{TagDto, TagName}
import org.http4s.HttpRoutes
import org.http4s.Status.{Created, NoContent, NotFound, Ok, UnprocessableEntity}
import org.http4s.circe.*
import org.http4s.circe.CirceEntityCodec.circeEntityEncoder
import org.http4s.dsl.Http4sDsl
import org.http4s.implicits.*
import org.http4s.syntax.*
import repositories.BlogsSkunk

// These are necessary to use for-comprehensions on F
import cats.syntax.flatMap.*
import cats.syntax.functor.*
//import cats.syntax.*
import cats.implicits.*


//import monocle.syntax.*
import monocle.Lens
import monocle.macros.GenLens
import monocle.macros.syntax.AppliedFocusSyntax

// The type constraint of Concurrent is necessary to decode Json
class BlogService[F[_]: Concurrent](repository: BlogsSkunk[F]) extends Http4sDsl[F] {

  object BlogIdVar:
    def unapply(str: String): Option[BlogId] = Some(BlogId(str))

  object OptionalTagQueryParamMatcher extends  OptionalQueryParamDecoderMatcher[String]("tag")

  val routes: HttpRoutes[F] = HttpRoutes.of[F] {
//    case GET -> Root => Ok(repository.findAllBlogs)


    case GET -> Root :? OptionalTagQueryParamMatcher(tag) => tag match {
      case Some(t) => {
        val tn = TagName(t)
        Ok(repository.findByTag(tn))
      }
      case None => Ok(repository.findAllBlogs)
    }

    case GET -> Root / BlogIdVar(id) =>
      for {
        blog <- repository.findBlogById(id)
        res <- blog.fold(NotFound())(Ok(_))
      } yield res

    case req @ POST -> Root =>
      for
        dto <- req.decodeJson[BlogDto]
        blog <- BlogDto.toDomain(dto).pure[F]
        res <- blog.fold(UnprocessableEntity(_), b => Created(repository.create(b)))
      yield res

    case req @ POST -> Root / BlogIdVar(id) / "addTag" =>
      for {
        dto <- req.asJsonDecode[TagDto]
        tag <- TagDto.toDomain(dto, id).pure[F]
        res <- Created(repository.addTag(tag))
      } yield res


    case req @ PUT -> Root / BlogIdVar(id) =>
      for {
        dto <- req.decodeJson[BlogDto]
        foundBlog <- repository.findBlogById(id)
        updatedBlog = BlogDto.toDomain(dto)
        res <- (foundBlog, updatedBlog) match
          case (None, _) => NotFound()
          case (_, Invalid(e)) => UnprocessableEntity(e)
          case (Some(b), Valid(u)) =>
            val blogTitle = Lens[Blog, BlogTitle](_.title)(t => b => b.copy(title = t))
            val blogContent = Lens[Blog, BlogContent](_.content)( c => b => b.copy(content = c))
            val newBlog = b.copy(title = u.title, content = u.content)
            val lensyBoi = blogTitle.replace(u.title)(b)
            Created(repository.update(newBlog))
      } yield res

    case DELETE -> Root / BlogIdVar(id) =>
      for {
        res <- repository.delete(id)
        y <- res.fold(NotFound())( _ => NoContent())
      } yield y
  }
}