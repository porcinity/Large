import cats.Monad
import cats.effect.{IO, Sync}
import cats.*
import org.http4s.{EntityDecoder, EntityEncoder}
//import cats.syntax.option._
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import org.http4s.circe._
import org.http4s.circe.CirceEntityCodec.circeEntityEncoder
import org.http4s.circe.CirceEntityEncoder.circeEntityEncoder
import repo.BlogsRepo.{Blog, Blogs, BlogId, TestSprout}
import io.circe.generic.auto.*
import io.circe.syntax.*
import io.circe.*
import repo.BlogsRepo.BlogId._


class BlogService(repository: Blogs[IO]) extends Http4sDsl[IO] {
  //  import io.circe._, io.circe.generic.semiauto._
  //
//    implicit val fooEncoder: Encoder[TestSprout] = deriveEncoder[TestSprout]
//    implicit val fooDecoder: Decoder[Blog] = deriveDecoder[Blog]

//  implicit val encoder: Encoder[TestSprout] = Encoder[TestSprout].contramap(x => x)
//  implicit val decoder: Decoder[TestSprout] = Decoder[String].map(TestSprout(_))
//  implicit val entityEncoder: EntityEncoder[IO, TestSprout] = jsonEncoderOf[IO, TestSprout]
//  implicit val entityDecoder: EntityDecoder[IO, TestSprout] = jsonOf[IO, TestSprout]

  val routes = HttpRoutes.of[IO] {
    case GET -> Root =>
      for {
        x <- repository.findAll
        y <- Ok(x)
      } yield y

    case GET -> Root / IntVar(id) =>
      for {
        b <- repository.findById(id)
        res <- b.fold(NotFound())(Ok(_))
      } yield res

    case GET -> Root / "rich" / IntVar(id) =>
      for {
        b <- repository.findRichIdById(id)
        res <- b.fold(NotFound())(Ok(_))
      } yield res
  }
}


