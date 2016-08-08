package leo.gui.agent_observer

import leo.gui.agent_observer.events.Event

/**
  *
  * ObserverPattern for the GUI.
  *
  * @since 8/4/16
  * @author Max Wisniewski
  */
trait Observe[T <: Event] {

  /**
    * Handles the occurence of an Event T
    * @param t
    */
  def handle(from : AnyRef, t : T) : Unit
}
