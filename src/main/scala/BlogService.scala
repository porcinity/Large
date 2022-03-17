import models.KewlBlog.*
import models.Person.*
import cats.Monad
import cats.effect.{Concurrent, IO, Sync}
import cats.*
import doobie.{Meta, Read}
import io.circe.generic.semiauto.deriveCodec
import org.http4s.Status.{Created, NoContent, Ok}
import org.http4s.{EntityDecoder, EntityEncoder}
import repo.BlogsRepo.Blogs
//import cats.syntax.option._
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import org.http4s.circe._
import org.http4s.circe.CirceEntityCodec.circeEntityEncoder
import org.http4s.circe.CirceEntityEncoder.circeEntityEncoder
import org.http4s.implicits._
import org.http4s.syntax._
import io.circe.syntax.*
import io.circe.*


// These are necessary to use for-comprehensions on F
import cats.syntax.flatMap._
import cats.syntax.functor._

// The type constraint of Concurrent is necessary to decode Json
class BlogService[F[_]: Concurrent](repository: Blogs[F]) extends Http4sDsl[F] {

//  implicit val personDecoder: EntityDecoder[F, Person] = jsonOf[F, Person]

  val peeps = List(
    Person(PersonId(1), Name("Anthony"), Age(32), Weight(180)),
    Person(PersonId(2), Name("Tess"), Age(24), Weight(140))
  )

  val kewlList = List(
    KewlBlog(KewlId(1), KewlTitle("Itb"), KewlContent("lol")),
    KewlBlog(KewlId(2), KewlTitle("itb pt 2"), KewlContent("sfdfs"))
  )

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
        _ <- repository.create(kewl.id.value, kewl.title.value, kewl.content.v)
        res <- Created()
      } yield res

    case DELETE -> Root / IntVar(id) =>
      for {
        res <- repository.delete(id)
        y <- res.fold(_ => NotFound(), _ => NoContent())
      } yield y
  }
}