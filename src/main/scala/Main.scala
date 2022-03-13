import cats.effect.{ExitCode, IO, IOApp, Resource}
import doobie.hikari.HikariTransactor
import doobie.util.ExecutionContexts

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
    
    val blogs: Blogs[IO] = Blogs.make(postgres)
    
    val program: IO[Unit] = for {
      blogs <- blogs.findAll
      _ <- IO.println(s"These are the blogs: $blogs")
    } yield ()
    
    program.as(ExitCode.Success)