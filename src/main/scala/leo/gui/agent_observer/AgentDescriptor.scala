package leo.gui.agent_observer

import javafx.scene.layout.{HBox, Pane, VBox}
import javafx.scene.control.Label
import javafx.scene.text.Font

import leo.agents.Agent
import leo.gui.agent_observer.events.WealthUpdateEvent

/**
  * Created by mwisnie on 8/4/16.
  */
class AgentDescriptor(a : Agent) extends VBox with Observe[WealthUpdateEvent] {


  val wealth : Label = new Label("0")

  def addToParent(parent : Pane): Unit = {

    // TODO add style

    val namel = new Label(a.name)

    val hbox = new HBox()
    val maxWealth = new Label(" / "+a.maxMoney)
    hbox.getChildren.addAll(wealth, maxWealth)

    parent.getChildren.addAll(namel, hbox)


    parent.getChildren.add(this)
  }

  /**
    * Handles the occurence of an Event T
    *
    * @param t
    */
  override def handle(from: AnyRef, t: WealthUpdateEvent): Unit = {
    assert(from.eq(a), "Only registered events should land here.")
    wealth.setText(t.money.round+"")
  }
}
