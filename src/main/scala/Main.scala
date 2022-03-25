import cats.effect.{ExitCode, IO, IOApp, Resource, Sync}
import doobie.hikari.HikariTransactor
import doobie.util.ExecutionContexts
import models.Blog.*
import org.http4s.HttpRoutes
import org.http4s.ember.server.*
import org.http4s.implicits.*
import org.http4s.syntax.all.*
import org.http4s.server.Router
import repositories.{AuthorsSkunk, BlogsSkunk, Users}
import routes.{AuthorService, BlogService}
import com.comcast.ip4s.{ipv4, port}
import skunk.*
import skunk.codec.text.*
import skunk.implicits.*
import natchez.Trace.Implicits.noop

object Main extends IOApp:
  override def run(args: List[String]): IO[ExitCode] =

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

    val session: Resource[IO, Resource[IO, Session[IO]]] =
      Session.pooled[IO](
        host = "localhost",
        port = 5432,
        user = "anthony",
        password = Some("itb"),
        database = "blog",
        max = 10
      )

//    val blogsRepo: Blogs[IO] = Blogs.make(postgres)

    val usersRepo: Users[IO] = Users.make(postgres)

    val skunkRepo: AuthorsSkunk[IO] = AuthorsSkunk.make(session)

    val blogsSkunk: BlogsSkunk[IO] = BlogsSkunk.make(session)

    val blogService: BlogService[IO] = new BlogService(blogsSkunk)

    val authorService: AuthorService[IO] = new AuthorService(skunkRepo)

    val httpApp = Router(
      "/blogs" -> blogService.routes,
      "/authors" -> authorService.routes
    ).orNotFound

    for
      server <- EmberServerBuilder
        .default[IO]
        .withHost(ipv4"0.0.0.0")
        .withPort(port"8080")
        .withHttpApp(httpApp)
        .build
        .use(_ => IO.never)
        .as(ExitCode.Success)
    yield server