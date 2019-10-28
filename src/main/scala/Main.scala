import scala.util.Try
import zio._
import zio.console._
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL.Parse._
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.model._

object Main extends App {
  def run(args: List[String]) = scraperLogic.fold(_ => 1, _ => 0)

  val domain = "https://zio.dev"
  
  val startPageUrl = s"${domain}/docs/overview/overview_index"

  val browser = JsoupBrowser()

  case class Topic(val name: String, val url: String)
  
  case class Subtopic(val name: String) {
    override def toString = s"- ${name}"
  }
  
  case class Section(val name: String, val subtopics: List[Subtopic]) {
    override def toString = s"# ${name}"
  }

  def scraperLogic: RIO[Console, Unit] =
    for {
      topics   <- fetchTopics(startPageUrl)
      sections <- fetchSections(topics)
      _        <- printSections(sections)
    } yield ()

  def fetchTopics(url: String): Task[List[Topic]] =
    for {
      doc    <- ZIO.fromTry(Try(browser.get(url)))
      topics <- Task.succeed(
        (doc >> elementList("#docsNav a")).map(e => Topic(e.text, s"${domain}${e.attr("href")}"))
      )
    } yield topics
  
  def fetchSections(topics: List[Topic]): Task[List[Section]] =
    ZIO.foreach(topics)(topic =>
      for {
        doc       <- ZIO.fromTry(Try(browser.get(topic.url)))
        texts     <- Task.succeed(doc >> texts(".toc-headings a"))
        subtopics <- Task.succeed(texts.map(Subtopic(_)).toList)
      } yield Section(topic.name, subtopics)
    )
  
  def printSections(sections: List[Section]): URIO[Console, List[Unit]] =
    ZIO.foreach(sections)(section =>
      for {
        _ <- putStrLn(section.toString)
        _ <- ZIO.foreach(section.subtopics)(s => putStrLn(s.toString))
      } yield ()
    )
}
