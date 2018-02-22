
import formats._
import fp.State
import spray.json._

import scala.io.Source
import scala.language.{higherKinds, implicitConversions, postfixOps}
import scala.util.{Failure, Success, Try}

object StateChallenge extends ConfigParser[StateConfig] with Serializations with Formats {
  type ComponentsState[A] = State[List[Component], A]
  type ComponentState[A] = State[Component, A]

  def main(args: Array[String]): Unit = (for {
    config            <- readConfig(args)
    dependencies      <- parseFile[Dependencies](config.initialFile)
    triggers          <- parseFile[Triggers](config.eventsFile)
    updatedComponents <- processEvents(triggers.events)(dependencies.graph.components)
  } yield updatedComponents) match {
    case Success(components) => println(Dependencies(Graph(components)).toJson.compactPrint)
    case Failure(err: IllegalArgumentException) => println(s"IllegalArgumentException $err")
    case Failure(err) => println(s"Some other error $err")
  }

  override def readConfig(args: Array[String]): Try[StateConfig] = for {
    _                <- checkSize(2)(args)
    initialStateFile <- readArg(0)(args)
    eventsFile       <- readArg(1)(args)
  } yield StateConfig(initialStateFile, eventsFile)

  private def parseFile[A](filename: String)(implicit s: Serialization[A]) = for {
    json <- loadFile(filename)
    data <- s.fromJson(json)
  } yield data

  private def loadFile(filename: String): Try[String] = Try { Source.fromFile(filename).getLines.mkString }

  private def processEvents(events: List[Event])(components: List[Component]): Try[List[Component]] = Try { applyEvents(events)(components) }

  private def applyEvents(events: List[Event])(components: List[Component]): List[Component] =
    events.sortBy(_.timestamp).foldLeft(components) {
      (componentsBuffer, event) => components.find(_.id == event.component) match {
        case Some(component) =>
          (for {
            updatedComponent  <- applyComponent(updateComponent(event, componentsBuffer).exec(component))
            updatedComponents <- updateDependenciesOf(updatedComponent)
          } yield updatedComponents).exec(componentsBuffer)

        case None            => componentsBuffer
      }
    }

  private def updateComponent(event: Event, components: List[Component]): ComponentState[Component] = for {
    _                <- updateCheckStates(event)
    _                <- updateOwnState
    _                <- updateDerivedState(components)
    updatedComponent <- State.get
  } yield updatedComponent

  private def applyComponent(component: Component): ComponentsState[Component] = State { components =>
    (component +: components.filterNot(_.id == component.id), component)
  }

  private def updateDependenciesOf(component: Component): ComponentsState[List[Component]] = State { components =>
    val newComponents = findRelatedComponents(components)(component.dependencyOf.getOrElse(List.empty)).foldLeft(components) {
      (cSS, comp) => applyComponent(updateDerivedState(cSS).exec(comp)).exec(cSS)
    }

    (newComponents, newComponents)
  }

  private def updateCheckStates(event: Event): ComponentState[Map[String, ComponentStateValue]] = State { component =>
    val newCheckStates = component.checkStates + (event.checkState -> event.state)
    (component.copy(checkStates = newCheckStates), newCheckStates)
  }

  private def updateOwnState: ComponentState[ComponentStateValue] = State { component =>
    val newOwnState = computeOwnState(component.checkStates)
    (component.copy(ownState = newOwnState), newOwnState)
  }

  private def updateDerivedState(components: List[Component]): ComponentState[ComponentStateValue] = State { component =>
    val newDerivedState = (derivedState(components) andThen propagateDerivedState)(component)
    (component.copy(derivedState = newDerivedState), newDerivedState)
  }

  private def applyEvent(event: Event, component: Component): Component = {
    val newCheckStates = component.checkStates + (event.checkState -> event.state)
    component.copy(checkStates = newCheckStates, ownState = computeOwnState(newCheckStates))
  }

  private def computeOwnState(checkStates: Map[String, ComponentStateValue]): ComponentStateValue = checkStates.values.toList match {
    case Nil    => NoData()
    case states => states.maxBy(_.prio)
  }

  private def derivedState(components: List[Component]): Component => ComponentStateValue = { component => (for {
      depComponentsState <- component.dependsOn.map(dependsOn => getDependsOnState(components)(dependsOn))
      if depComponentsState >= component.ownState
    } yield depComponentsState) getOrElse component.ownState
  }

  private def propagateDerivedState: ComponentStateValue => ComponentStateValue = { derivedComponentState =>
    if (derivedComponentState >= Warning()) derivedComponentState else NoData()
  }

  private def getDependsOnState(components: List[Component]): List[String] => ComponentStateValue =
    (findRelatedComponents(components) andThen calculateDependsOnState)(_)

  private def calculateDependsOnState: List[Component] => ComponentStateValue = _.map(_.derivedState).maxBy(_.prio)

  private def findRelatedComponents(components: List[Component]): List[String] => List[Component] =
    _.flatMap(dependencyId => components.find(_.id == dependencyId))
}
