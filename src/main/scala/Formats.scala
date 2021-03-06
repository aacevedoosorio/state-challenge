import spray.json.DefaultJsonProtocol._
import spray.json._

import scala.language.{higherKinds, implicitConversions, postfixOps}
import scala.util.Try

trait Serializations  { self: Formats =>
  private def serializer[M](implicit j: RootJsonFormat[M]) = new Serialization[M] {
    override def toJson(m: M): Try[String] = Try { m.toJson.compactPrint }

    override def fromJson(json: String): Try[M] = Try { json.parseJson.convertTo[M] }
  }

  final implicit protected def graphSerializer: Serialization[Application] = serializer[Application]
  final implicit protected def eventsSerializer: Serialization[Triggers] = serializer[Triggers]
}

trait Serialization[M] {
  def toJson(m: M): Try[String]
  def fromJson(json: String): Try[M]
}

trait Formats {
  implicit val componentState: RootJsonFormat[ComponentStateValue] = new RootJsonFormat[ComponentStateValue] {
    override def write(state: ComponentStateValue): JsValue = JsString(state.name)

    override def read(json: JsValue): ComponentStateValue = json match {
      case JsString(name) => ComponentStateValue.findBy(name) match {
        case Some(state) => state
        case None => deserializationError("Invalid component state")
      }
      case _ => deserializationError("Missing component state")
    }
  }

  implicit val componentFormat: RootJsonFormat[Component] = jsonFormat(Component, "id", "own_state", "derived_state", "check_states",
    "depends_on", "dependency_of")
  implicit val graphFormat: RootJsonFormat[Graph] = jsonFormat1(Graph.apply)
  implicit val dependenciesFormat: RootJsonFormat[Application] = jsonFormat1(Application.apply)

  implicit val eventFormat: RootJsonFormat[Event] = jsonFormat(Event, "timestamp", "component", "check_state", "state")
  implicit val triggersFormat: RootJsonFormat[Triggers] = jsonFormat1(Triggers.apply)
}

sealed trait ComponentStateValue {
  def name: String
  def prio: Int
  def >=(componentState: ComponentStateValue): Boolean = this.prio >= componentState.prio
}

case class NoData(override val name: String = "no_data", override val prio: Int = 0) extends ComponentStateValue
case class Clear(override val name: String = "clear", override val prio: Int = 1) extends ComponentStateValue
case class Warning(override val name: String = "warning", override val prio: Int = 2) extends ComponentStateValue
case class Alert(override val name: String = "alert", override val prio: Int = 3) extends ComponentStateValue

object ComponentStateValue {
  private val allowedStates = Seq(NoData(), Clear(), Warning(), Alert())
  def findBy(name: String): Option[ComponentStateValue] = allowedStates.find(_.name == name)
}

case class Application(graph: Graph)
case class Graph(components: List[Component])
case class Component(id : String, ownState: ComponentStateValue, derivedState: ComponentStateValue, checkStates: Map[String, ComponentStateValue],
                     dependsOn: Option[List[String]], dependencyOf: Option[List[String]])

case class Triggers(events: List[Event])
case class Event(timestamp: String, component: String, checkState: String, state: ComponentStateValue)