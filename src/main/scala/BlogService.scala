import models.KewlBlog.*
import models.Person.*
import cats.Monad
import cats.effect.{IO, Sync}
import cats.*
import doobie.{Meta, Read}
import io.circe.generic.semiauto.deriveCodec
import org.http4s.{EntityDecoder, EntityEncoder}
import repo.BlogsRepo.Blogs
//import cats.syntax.option._
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import org.http4s.circe._
import org.http4s.circe.CirceEntityCodec.circeEntityEncoder
import org.http4s.circe.CirceEntityEncoder.circeEntityEncoder
import io.circe.syntax.*
import io.circe.*


class BlogService(repository: Blogs[IO]) extends Http4sDsl[IO] {
  implicit val personDecoder: EntityDecoder[IO, Person] = jsonOf[IO, Person]

  val peeps = List(
    Person(PersonId(1), Name("Anthony"), Age(32), Weight(180)),
    Person(PersonId(2), Name("Tess"), Age(24), Weight(140))
  )

  val kewlList = List(
    KewlBlog(KewlId(1), KewlTitle("Itb"), KewlContent("lol")),
    KewlBlog(KewlId(2), KewlTitle("itb pt 2"), KewlContent("sfdfs"))
  )

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case GET -> Root / "peeps" => Ok(peeps)
    case req @ POST -> Root / "peeps" =>
      for {
        user <- req.decodeJson[Person]
        resp <- Ok(peeps.concat(List(user)))
      } yield resp

    case DELETE -> Root / "peeps" / IntVar(id) => Ok(peeps.filter(_.id.value != id))

    case GET -> Root / "peeps" / IntVar(id) => Ok(peeps.find(_.id.value == id))

    case GET -> Root / "kewl" => Ok(kewlList)
    case GET -> Root / "kewl" / IntVar(id) =>
      for{
        x <- Ok(kewlList.find(_.id.value == id))
      } yield x

    case GET -> Root =>
      for {
        x <- repository.findAllKewlBlogs
        y <- Ok(x)
      } yield y

    case GET -> Root / IntVar(id) =>
      for {
        b <- repository.findKewlBlogById(id)
        res <- b.fold(NotFound())(Ok(_))
      } yield res
  }
}