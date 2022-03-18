import cats.effect.{ExitCode, IO, IOApp, Resource, Sync}
import doobie.hikari.HikariTransactor
import doobie.util.ExecutionContexts
import models.KewlBlog.*
import org.http4s.HttpRoutes
import org.http4s.Status.Ok
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.dsl.io.{GET, Root}
import org.http4s.server.Router
import repositories.Blogs

object Main extends IOApp:
  override def run(args: List[String]): IO[ExitCode] =

    import scala.concurrent.ExecutionContext.global

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

    val blogsRepo: Blogs[IO] = Blogs.make(postgres)

    val blogService: BlogService[IO] = new BlogService(blogsRepo)

    val httpApp = Router(
      "/blogs" -> blogService.routes
    ).orNotFound

    for {
      server <- BlazeServerBuilder[IO](global)
        .bindHttp(8080, "localhost")
        .withHttpApp(httpApp)
        .resource
        .use(_ => IO.never)
        .as(ExitCode.Success)
    } yield server