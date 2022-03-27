import cats.effect.{ExitCode, IO, IOApp, Resource, Sync}
import doobie.hikari.HikariTransactor
import doobie.util.ExecutionContexts
import models.Blog.*
import org.http4s.HttpRoutes
import org.http4s.ember.server.*
import org.http4s.implicits.*
import org.http4s.syntax.all.*
import org.http4s.server.Router
import repositories.{BlogsSkunk, Users, UsersSkunk}
import routes.{BlogService, UserService}
import com.comcast.ip4s.{ipv4, port}
import skunk.*
import skunk.codec.text.*
import skunk.implicits.*
import natchez.Trace.Implicits.noop
import org.http4s.server.middleware.Logger

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

    val skunkRepo: UsersSkunk[IO] = UsersSkunk.make(session)

    val blogsSkunk: BlogsSkunk[IO] = BlogsSkunk.make(session)

    val blogService: BlogService[IO] = new BlogService(blogsSkunk)

    val userService: UserService[IO] = new UserService(skunkRepo)

    val httpApp = Router(
      "/blogs" -> blogService.routes,
      "/users" -> userService.routes
    ).orNotFound

    val finalHttpApp = Logger.httpApp(true, true)(httpApp)

    for
      server <- EmberServerBuilder
        .default[IO]
        .withHost(ipv4"0.0.0.0")
        .withPort(port"8080")
        .withHttpApp(finalHttpApp)
        .build
        .use(_ => IO.never)
        .as(ExitCode.Success)
    yield server