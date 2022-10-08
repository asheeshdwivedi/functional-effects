package net.degoes.zio

import zio._

/**
 * ZIO environment is a type-indexed map that allows you to store a number of
 * objects of different types. ZIO calls these objects "services", because
 * they contain bundles of functionality consumed your application.
 */
object TypeIndeedMap extends ZIOAppDefault {
  trait Logging
  object Logging extends Logging

  trait Database
  object Database extends Database

  trait Cache
  object Cache extends Cache

  val envLogging = ZEnvironment(Logging: Logging)

  val envDatabase = ZEnvironment(Database: Database)

  val envCache = ZEnvironment(Cache: Cache)

  val empty = ZEnvironment.empty

  /**
   * EXERCISE
   *
   * Using the `++` operator on `ZEnvironment`, combine the three maps
   * (`envLogging`, `envDatabase`, and `envCache`) into a single map that
   * has all three objects.
   */
  val allThree: ZEnvironment[Database with Cache with Logging] =
    envLogging ++ envDatabase ++ envCache

  final case class Second[A: Tag](value: A)

  final case class OldDatabase(database: Database)

  /**
   * EXERCISE
   *
   * Using `ZEnvironment#get`, which can retrieve an object stored in
   * the map, retrieve the logging, database, and cache objects from
   * `allThree`. Note that you will have to specify the type parameter,
   * as it cannot be inferred (the map needs to know which of the objects
   * you want to retrieve, and that can be specified only by type).
   */
  lazy val logging  = allThree.get[Logging]
  lazy val database = allThree.get[Database]
  lazy val cache    = allThree.get[Cache]
  lazy val otherCopyOfDatabase =
    (allThree ++ ZEnvironment(Second[Database](database): Second[Database])).get[Second[Database]].value
  //lazy val int = allThree.get[Int]

  val run = Console.printLine(allThree)
}

object AccessEnvironment extends ZIOAppDefault {

  // mental model
  // ZIO[R, E, A]
  // ZEnvironment[R] => Either[E, A]

  final case class Config(host: String, port: Int)

  /**
   * EXERCISE
   *
   * Using `ZIO.service`, access a `Config` service from the environment, and
   * extract the `host` field from it.
   */
  val accessHost: ZIO[Config, Nothing, String] =
    for {
      config <- ZIO.service[Config]
    } yield config.host

  /**
   * EXERCISE
   *
   * Using `ZIO.service`, access a `Config` service from the environment, and
   * extract the `port` field from it.
   */
  val accessPort: ZIO[Config, Nothing, Int] =
    for {
      config <- ZIO.service[Config]
    } yield config.port

  val run = {
    val config = Config("localhost", 7878)

    (for {
      host <- accessHost
      port <- accessPort
      _    <- Console.printLine(s"Configuration: ${host}:${port}")
    } yield ()).provideEnvironment(ZEnvironment(config))
  }
}

object ProvideEnvironment extends ZIOAppDefault {

  final case class Config(server: String, port: Int)

  final case class DatabaseConnection() {
    def query(query: String): Task[Int] = ZIO.attempt(42)
  }

  val getServer: ZIO[Config, Nothing, String] =
    ZIO.service[Config].map(_.server)

  val useDatabaseConnection: ZIO[DatabaseConnection, Throwable, Int] =
    ZIO.serviceWithZIO[DatabaseConnection](_.query("SELECT * FROM USERS"))

  /**
   * EXERCISE
   *
   * Compose both the `getServer` and `useDatabaseConnection` effects together
   * and run them.
   * In order to do this successfully, you will have to use
   * `ZIO#provideEnvironment` to give them the environment that they need in
   * order to run.
   */
  val run = {
    val config = Config("localhost", 7878)

    val effect = for {
      server     <- getServer
      _          <- Console.printLine(s"Server $server")
      connection <- useDatabaseConnection
      _          <- Console.printLine(s"Connection $connection")
    } yield ()

    effect
      .provideEnvironment(ZEnvironment[Config, DatabaseConnection](config, DatabaseConnection()))
  }
}

/**
 * In ZIO, layers are values that contain construction logic for services in
 * your  application. Services provide functionality like persistence or
 * logging or authentication, and they are used by business logic.
 *
 * A layer is a lot like a constructor, but may have complex initialization
 * or finalization, or may produce more than one service.
 *
 * ZIO has compile-time, type-safe wiring up of layers, which allows you to
 * optionally use ZIO for dependency-injection. The wire-up of layers
 * is done in a resource-safe, failure-aware way, with maximum parallelism
 * to decrease application startup time.
 *
 * Layers bring more power and compositionality to constructors. Although you
 * don't have to make your own layers to benefit from ZIO, layers can make
 * it easier and safer to assemble applications out of modules.
 */
object LayerEnvironment extends ZIOAppDefault {

  import java.io.IOException

  type MyFx = Logging with Files

  trait Files {
    def read(file: String): IO[IOException, String]
  }
  class FilesLive extends Files {
    override def read(file: String): IO[IOException, String] = ZIO.attemptBlockingIO("File")
  }
  object Files {

    /**
     * EXERCISE
     *
     * Using `ZLayer.succeed`, create a layer that implements the `Files`
     * service.
     */
    val live: ZLayer[Any, Nothing, Files] = ZLayer.succeed(new FilesLive())
  }

  trait Logging {
    def log(line: String): UIO[Unit]
    def destroy(): UIO[Unit]
  }

  object Logging {

    class LoggingLive(console: Console, ref: Ref[Int]) extends Logging {

      override def log(line: String): UIO[Unit] = ref.update(_ + 1) *> console.printLine(s"INFO $line").orDie

      override def destroy(): UIO[Unit] = console.printLine("Destroying the logging").ignore
    }

    def create(console: Console, ref: Ref[Int]): Logging = new LoggingLive(console, ref)

    /**
     * EXERCISE
     *
     * Using `ZLayer.fromFunction`, create a layer that requires `Console`
     * and uses the console to provide a logging service.
     */
    val live: ZLayer[Console, Nothing, Logging] = {
      val zlayer1 = ZLayer.fromFunction(create(_, _))
      val zlayer2 = ZLayer { // classes that do not use resources
        for {
          ref     <- Ref.make(0)
          console <- ZIO.service[Console]
        } yield new LoggingLive(console, ref)
      }
      val zlayer3 = ZLayer.scoped {
        for {
          ref     <- Ref.make(0)
          console <- ZIO.service[Console]
          logging <- ZIO.succeed(new LoggingLive(console, ref))
          _       <- ZIO.addFinalizer(logging.destroy)
        } yield logging
      }

      zlayer3
    }
  }

  /**
   * EXERCISE
   *
   * Discover the inferred type of `effect`, and write it out explicitly.
   */
  val effect2 =
    for {
      files   <- ZIO.service[Files]
      logging <- ZIO.service[Logging]
      file    <- files.read("build.sbt")
      _       <- logging.log(file)
    } yield ()

  val run = {

    /**
     * EXERCISE
     *
     * Create a layer using `ZLayer.make` and specifying all the pieces that go into the layer.
     */
    val fullLayer: ZLayer[Any, Nothing, Files with Logging] =
      ZLayer.make[Files with Logging](
        Files.live,
        Logging.live,
        ZLayer.succeed(Console.ConsoleLive)
      )

    /**
     * EXERCISE
     *
     * Using `ZIO#provide`, provide the full layer into the effect to remove its dependencies.
     */
    val effect: ZIO[Any, IOException, Unit] = effect2.provide(fullLayer)

    effect
  }
}
