package leo.gui.agent_observer

import javafx.scene.layout.{HBox, Pane}

import leo.agents.Agent

/**
  * Created by mwisnie on 8/4/16.
  */
class AgentContainer(a : Agent) extends HBox {

  val agent : AgentDescriptor = new AgentDescriptor(a)
  val tasks : AgentTaskDescriptor = new AgentTaskDescriptor
  val filter : AgentFilterDescriptor = new AgentFilterDescriptor

  def addToParent(parent : Pane) = {

  }
}
