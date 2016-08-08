package leo.gui.agent_observer.events

trait Event {}

case class WealthUpdateEvent(money : Double) extends Event
case class Filter