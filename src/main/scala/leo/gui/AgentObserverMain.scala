package leo.gui

import javafx.application.Application
import javafx.scene.Scene
import javafx.scene.control.Label
import javafx.stage.Stage
import javafx.scene.layout.VBox

/**
  *
  * MainWindow for the Agent Observer GUI
  * @since 8/4/16
  * @author Max Wisniewski
  */
class AgentObserverMain extends Application {
  override def start(stage: Stage): Unit = {
    stage.setTitle("App")

    val root = new VBox()
    root.getChildren.add(new Label("Hallo World"))
    stage.setScene(new Scene (root, 300, 300))
    stage.show()
  }
}

object AgentObserverMain {
  def main(args: Array[String]): Unit = {
    Application.launch(classOf[AgentObserverMain], args: _*)
  }
}
