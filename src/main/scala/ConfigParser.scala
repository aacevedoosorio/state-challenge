import scala.util.{Failure, Success, Try}

trait ConfigParser[C <: AppConfig] {
  def readConfig(args: Array[String]): Try[C]

  final protected def readArg(at: Int): Array[String] => Try[String] = { args => Try { args(at) } }
  final protected def checkSize(total: Int): Array[String] => Try[Int] = {
    case arr if arr.length == total => Success(total)
    case _                          => Failure(new IllegalArgumentException("Configuration missing"))
  }
}

trait AppConfig
case class StateConfig(initialFile: String, eventsFile: String) extends AppConfig
