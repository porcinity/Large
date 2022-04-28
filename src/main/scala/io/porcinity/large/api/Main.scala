package io.porcinity.large.api

import cats.effect.{ExitCode, IO, IOApp, Resource}
import com.comcast.ip4s.Literals.ipv4
import io.porcinity.large.persistence.{Articles, Tags, Users}
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.Router
import org.http4s.server.middleware.Logger
import skunk.Session
import natchez.Trace.Implicits.noop
import com.comcast.ip4s.{ipv4, port}

object Main extends IOApp:
  override def run(args: List[String]): IO[ExitCode] =
    val session = Session.single[IO](
      host = "localhost",
      port = 5432,
      user = "pigg",
      password = Some("test"),
      database = "Large"
    )

    val userRepo    = Users.make[IO](session)
    val articleRepo = Articles.make[IO](session)
    val tagsRepo    = Tags.make[IO](session)

    val articleService: ArticlesRoutes[IO] = new ArticlesRoutes[IO](articleRepo)
    val userService: UsersRoutes[IO] =
      new UsersRoutes[IO](userRepo, articleRepo)
    val tagsService: TagsRoutes[IO] = new TagsRoutes[IO](tagsRepo)

    val httpApp = Router(
      "api/articles" -> articleService.routes,
      "api/users"    -> userService.routes,
      "api/tags"     -> tagsService.routes
    ).orNotFound

    val finalHttpApp = Logger.httpApp(true, true)(httpApp)

    for {
      server <- EmberServerBuilder
        .default[IO]
        .withHost(ipv4"0.0.0.0")
        .withPort(port"8080")
        .withHttpApp(finalHttpApp)
        .build
        .use(_ => IO.never)
        .as(ExitCode.Success)
    } yield server
