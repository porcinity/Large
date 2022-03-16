import models.KewlBlog.*
import models.Person.*
import cats.Monad
import cats.effect.{IO, Sync}
import cats.*
import doobie.{Meta, Read}
import io.circe.generic.semiauto.deriveCodec
import org.http4s.{EntityDecoder, EntityEncoder}
import repo.BlogsRepo.{Blogs}
//import cats.syntax.option._
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import org.http4s.circe._
import org.http4s.circe.CirceEntityCodec.circeEntityEncoder
import org.http4s.circe.CirceEntityEncoder.circeEntityEncoder
import io.circe.syntax.*
import io.circe.*


class BlogService(repository: Blogs[IO]) extends Http4sDsl[IO] {
  //    import io.circe._, io.circe.generic.semiauto._

//  val richList = List(
//    RichBlog(BlogId(1), Title("hello"), Content("hey")),
//    RichBlog(BlogId(2), Title("heyyy"), Content("hello"))
//  )


  //  case class Name(value: String)
  //  case class Age(value: Int)


  //  implicit val personEncoder: Encoder[Person] = person => Json.obj(
  //    "name" -> person.name.asJson,
  //    "age" -> person.age.asJson,
  //    "weightitb" -> person.weight.asJson
  //  )
  //
//    implicit val personDecoder: Decoder[Person] = Decoder.forProduct4("id", "name", "age", "weight")()
  implicit val personDecoder: EntityDecoder[IO, Person] = jsonOf[IO, Person]

  val peeps = List(
    Person(PersonId(1), Name("Anthony"), Age(32), Weight(180)),
    Person(PersonId(2), Name("Tess"), Age(24), Weight(140))
  )

  val kewlList = List(
    KewlBlog(KewlId(1), KewlTitle("Itb"), KewlContent("lol")),
    KewlBlog(KewlId(2), KewlTitle("itb pt 2"), KewlContent("sfdfs"))
  )




//  implicit val blogIdCode: Codec[BlogId] = deriveCodec[BlogId]
//  implicit val blogCodec: Codec[RichBlog] = deriveCodec[RichBlog]


  //
  //  implicit val fooDecoder: Decoder[Name] = deriveDecoder[Name]
  //  implicit val fooEncoder: Encoder[Name] = deriveEncoder[Name]
  //
  //
  //  implicit val Decoder: Decoder[Person] = deriveDecoder[Person]
  //  implicit val Encoder: Encoder[Person] = deriveEncoder[Person]

  //    implicit val fooEncoder: Encoder[RichBlog] = deriveEncoder[RichBlog]
  //    implicit val fooDecoder: Decoder[RichBlog] = deriveDecoder[RichBlog]

  //      implicit val fooEncoder: Encoder[RichBlog] = jsonOf
  //      implicit val fooDecoder: Decoder[RichBlog] = jsonOf

  //  implicit val encoder: Encoder[TestSprout] = Encoder[TestSprout].contramap(x => x)
  //  implicit val decoder: Decoder[TestSprout] = Decoder[String].map(TestSprout(_))
  //  implicit val entityEncoder: EntityEncoder[IO, TestSprout] = jsonEncoderOf[IO, TestSprout]
  //  implicit val entityDecoder: EntityDecoder[IO, TestSprout] = jsonOf[IO, TestSprout]

  //  implicit val natMeta: Meta[BlogId] = Meta[Int].imap(fromInt)(toInt)


  //  implicit val richBlogRead: Read[RichBlog] =
  //    Read[(Int, String, String)].map { case (id, title, content) => RichBlog(BlogId(id), Title(title), Content(content)) }
  //
  val routes = HttpRoutes.of[IO] {
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
//
//    case GET -> Root / "rich" / IntVar(id) =>
//      for {
//        b <- repository.findRichIdById(id)
//        c <- b.fold(NotFound())(x => Ok(x.value))
//      } yield c
//
//    case GET -> Root / "rich" => Ok(peeps)
//      for {
//        x <- Ok(peeps.map(personDto))
////        y <- Ok(x)
//      } yield x
//
//    case GET -> Root / "richBlogs" =>
//      for {
//        w <- repository.findAllRichBlogs()
//        x <- Ok(w.map(blogToDto))
//        //        y <- Ok(x)
//      } yield x



//}


