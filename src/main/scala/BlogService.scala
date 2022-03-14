import cats.Monad
import cats.effect.{IO, Sync}
import cats._
import cats.syntax.option._
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
//import org.http4s.circe._
import org.http4s.circe.CirceEntityCodec.circeEntityEncoder
import org.http4s.circe.CirceEntityEncoder.circeEntityEncoder
import repo.BlogsRepo.{Blog, Blogs}
import io.circe.generic.auto.*
import io.circe.syntax.*
import io.circe.*

class BlogService(repository: Blogs[IO]) extends Http4sDsl[IO] {
  //  import io.circe._, io.circe.generic.semiauto._
  //
  //  implicit val fooEncoder: Encoder[Blog] = deriveEncoder[Blog]
  //  implicit val fooDecoder: Decoder[Blog] = deriveDecoder[Blog]

  val routes = HttpRoutes.of[IO] {
    case GET -> Root => for {
      x <- repository.findAll
      y <- Ok(x)
    } yield y

    case GET -> Root / IntVar(id) =>
      val res = for {
        b <- repository.findById(id)
        res = b.fold(NotFound())(x =>Ok(x))
      } yield b.getOrElse(Blog(33, "", ""))
      Ok(res)
  }


  //      repository.findById(1).flatMap(x => x.fold(NotFound)(x=>Ok(x)))

  //      for {
  //      l <- repository.findById(1)
  //      res = l.map(_.toJson)
  //    } yield res
}

