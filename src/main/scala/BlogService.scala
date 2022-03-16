import models.KewlBlog.*
import models.Person.*
import cats.Monad
import cats.effect.{Concurrent, IO, Sync}
import cats.*
import doobie.{Meta, Read}
import io.circe.generic.semiauto.deriveCodec
import org.http4s.Status.Ok
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

    case GET -> Root / "peeps" => Ok(peeps)
    case req @ POST -> Root / "peeps" =>
      for {
        user <- req.decodeJson[Person]
        resp <- Ok(peeps.concat(List(user)))
      } yield resp

    case DELETE -> Root / "peeps" / IntVar(id) => Ok(peeps.filter(_.id.value != id))

    case GET -> Root / "peeps" / IntVar(id) => Ok(peeps.find(_.id.value == id))

    case GET -> Root => Ok(repository.findAllKewlBlogs)

    case GET -> Root / IntVar(id) => Ok(repository.findKewlBlogById(id))
  }
}