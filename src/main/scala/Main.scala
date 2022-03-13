import cats.effect.{ExitCode, IO, IOApp, Resource, Sync}
import doobie.hikari.HikariTransactor
import doobie.util.ExecutionContexts
import org.http4s.HttpRoutes
import org.http4s.Status.Ok
import org.http4s.dsl.io.{GET, Root}
import repo.BlogsRepo.Blogs

object Main extends IOApp:
  override def run(args: List[String]): IO[ExitCode] =

    def blogRoutes[F[_]: Sync](blogs: Blogs[F]): HttpRoutes[F] = {

      import org.http4s.circe.CirceEntityCodec.circeEntityEncoder
      import org.http4s.circe.CirceEntityEncoder.circeEntityEncoder

      HttpRoutes.of[F] {
        case GET -> Root => Ok(blogs.findAll)
      }
    }

    val postgres: Resource[IO, HikariTransactor[IO]] = for {
      ce <- ExecutionContexts.fixedThreadPool[IO](32)
      xa <- HikariTransactor.newHikariTransactor[IO](
        "org.postgresql.Driver",
        "jdbc:postgresql:blog",
        "anthony",
        "itb",
        ce
      )
    } yield xa

    val blogs: Blogs[IO] = Blogs.make(postgres)

    val program: IO[Unit] = for {
      blogs <- blogs.findAll()
      _ <- IO.println(s"These are the blogs: $blogs")
    } yield ()

    program.as(ExitCode.Success)