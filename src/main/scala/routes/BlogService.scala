package routes

import cats.data.Validated.{Invalid, Valid}
import cats.effect.Concurrent
import models.Blog.*
import org.http4s.HttpRoutes
import org.http4s.Status.{Created, NoContent, NotFound, Ok, UnprocessableEntity}
import org.http4s.circe.*
import org.http4s.circe.CirceEntityCodec.circeEntityEncoder
import org.http4s.dsl.Http4sDsl
import org.http4s.implicits.*
import org.http4s.syntax.*
import repositories.{Blogs, BlogsSkunk}

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
class BlogService[F[_]: Concurrent](repository: Blogs[F], otherBlog: BlogsSkunk[F]) extends Http4sDsl[F] {

  object BlogIdVar:
    def unapply(str: String): Option[String] = Some(str)

  val routes: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root => Ok(otherBlog.findAllBlogs)

    case GET -> Root / BlogIdVar(id) =>
      for {
        blog <- otherBlog.findBlogById(BlogId(id))
        res <- blog.fold(NotFound())(Ok(_))
      } yield res

    case req @ POST -> Root =>
      for
        dto <- req.decodeJson[BlogDto]
        blog <- BlogDto.toDomain(dto).pure[F]
        res <- blog.fold(UnprocessableEntity(_), b => Created(repository.insertBlog(b)))
      yield res

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
            Created(repository.update(lensyBoi))
      } yield res

    case DELETE -> Root / BlogIdVar(id) =>
      for {
        res <- repository.deleteBlog(id)
        y <- res.fold(_ => NotFound(), _ => NoContent())
      } yield y
  }
}