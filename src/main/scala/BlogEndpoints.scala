//import cats.Monad
//import cats.effect.Sync
//import org.http4s.circe.jsonOf
//import org.http4s.{EntityDecoder, HttpRoutes}
//import org.http4s.dsl.Http4sDsl
//import org.http4s.circe.CirceEntityCodec.circeEntityEncoder
//import org.http4s.circe.CirceEntityEncoder.circeEntityEncoder
//
//trait BlogRepositoryAlgebra[F[_]]:
//  def get(): F[Blog]
//
//class BlogService[F[_]](blogRepo: BlogRepositoryAlgebra[F]):
//  def findAll: F[Blog] =
//    blogRepo.get()
//
//object BlogService:
//  def apply[F[_]](
//                   repository: BlogRepositoryAlgebra[F]
//                 ): BlogService[F] = new BlogService[F](repository)
//
//class BlogEndpoints[F[_]: Monad] extends Http4sDsl[F]:
//
//  implicit val blogDecoder: EntityDecoder[F, Blog] = jsonOf
//
//  private def listBlogsEndpoint(blogService: BlogService[F]) = {
//        HttpRoutes.of[F] {
//          case GET -> Root => Ok(blogService.findAll)
//        }
//  }