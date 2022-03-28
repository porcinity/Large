import cats.effect.{ExitCode, IO, IOApp, Resource}
import org.http4s.HttpRoutes
import org.http4s.ember.server.*
import org.http4s.implicits.*
import org.http4s.syntax.all.*
import org.http4s.server.Router
import repositories.{Notes, Tags, Users}
import routes.{NotesRoutes, TagsRoutes, UserService}
import com.comcast.ip4s.{ipv4, port}
import skunk.*
import skunk.codec.text.*
import skunk.implicits.*
import natchez.Trace.Implicits.noop
import org.http4s.server.middleware.Logger

object Main extends IOApp:
  override def run(args: List[String]): IO[ExitCode] =

    val session: Resource[IO, Resource[IO, Session[IO]]] =
      Session.pooled[IO](
        host = "localhost",
        port = 5432,
        user = "anthony",
        password = Some("itb"),
        database = "Notes",
        max = 10
      )

    val usersRepo: Users[IO] = Users.make(session)

    val notesRepo: Notes[IO] = Notes.make(session)

    val tagsRepo: Tags[IO] = Tags.make(session)

    val notesService: NotesRoutes[IO] = new NotesRoutes(notesRepo)

    val userService: UserService[IO] = new UserService(usersRepo)

    val tagsService: TagsRoutes[IO] = new TagsRoutes[IO](tagsRepo)

    val httpApp = Router(
      "/notes" -> notesService.routes,
      "/users" -> userService.routes,
      "/tags" -> tagsService.routes
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