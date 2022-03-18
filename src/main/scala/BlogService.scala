import models.KewlBlog.*
import cats.effect.Concurrent
import org.http4s.Status.{Created, NoContent, Ok}
import org.http4s.{EntityDecoder, EntityEncoder}
import repo.BlogsRepo.Blogs
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import org.http4s.circe._
import org.http4s.implicits._
import org.http4s.syntax._
import org.http4s.circe.CirceEntityCodec.circeEntityEncoder
import io.circe.syntax.*
import io.circe.*

//import cats.syntax.option._
//import cats.*
//import doobie.{Meta, Read}
//import cats.Monad
//import org.http4s.circe.CirceEntityEncoder.circeEntityEncoder
//import io.circe.generic.semiauto.deriveCodec

// These are necessary to use for-comprehensions on F
import cats.syntax.flatMap._
import cats.syntax.functor._

// The type constraint of Concurrent is necessary to decode Json
class BlogService[F[_]: Concurrent](repository: Blogs[F]) extends Http4sDsl[F] {
  val routes: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root => Ok(repository.findAllKewlBlogs)

    case GET -> Root / IntVar(id) =>
      for {
        blog <- repository.findKewlBlogById(id)
        res <- blog.fold(NotFound())(Ok(_))
      } yield res

    case req @ POST -> Root =>
      for {
        kewl <- req.decodeJson[KewlBlog]
        newId <- repository.create(kewl.id.value, kewl.title.value, kewl.content.v)
        res <- Created(newId)
      } yield res

    case req @ PUT -> Root / IntVar(id) =>
      for {
        kewl <- req.decodeJson[KewlBlog]
        newId <- repository.update(kewl.id.value, kewl.title.value, kewl.content.v)
        res <- Created(newId)
      } yield res

    case DELETE -> Root / IntVar(id) =>
      for {
        res <- repository.delete(id)
        y <- res.fold(_ => NotFound(), _ => NoContent())
      } yield y
  }
}