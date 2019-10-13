import scala.util.{Random, Try}
import zio.{App, Task, RIO, ZIO}
import zio.console._

object Main extends App {
  def run(args: List[String]) = guessLogic.fold(_ => 1, _ => 0)

  def guessLogic: RIO[Console, Unit] =
    for {
      _    <- putStrLn("Hello! What is your name?")
      name <- getStrLn
      _    <- putStrLn(s"Hello, ${name}, welcome to the game!")
      _    <- gameLoop(name)
    } yield ()

  def gameLoop(name: String): RIO[Console, Unit] =
    for {
      num   <- nextInt(5).map(_ + 1)
      _     <- putStrLn(s"Dear ${name}, please guess a number from 1 to 5:")
      input <- getStrLn
      _     <- printResults(input, num, name)
      cont  <- checkContinue(input)
      _     <- if (cont) gameLoop(name) else Task.succeed(())
    } yield ()

  def parseInt(s: String): Option[Int] = Try(s.toInt).toOption

  def nextInt(upper: Int): Task[Int] = ZIO.effect(Random.nextInt(upper))

  def printResults(input: String, num: Int, name: String) =
    parseInt(input).fold(
      putStrLn("You did not enter a number")
    )(guess =>
      if (guess == num) putStrLn("You guessed right, " + name + "!")
      else putStrLn("You guessed wrong, " + name + "! The number was: " + num)
    )

  def checkContinue(name: String): RIO[Console, Boolean] =
    for {
      _     <- putStrLn(s"Do you want to continue, ${name}?")
      input <- getStrLn.map(_.toLowerCase)
      cont  <- input match {
                 case "y" => Task.succeed(true)
                 case "n" => Task.succeed(false)
                 case _   => checkContinue(input)
               }
    } yield cont
}