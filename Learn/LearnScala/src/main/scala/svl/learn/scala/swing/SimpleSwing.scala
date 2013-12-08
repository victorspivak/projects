package svl.learn.scala.swing

import scala.swing._
import scala.swing.event.ButtonClicked
import java.awt.event.{ComponentEvent, ComponentListener, ActionEvent, ActionListener}

object SimpleSwing extends SimpleSwingApplication {
    val button1 = new Button{
        text = "Click me"
    }

    button1.subscribe{
        case ButtonClicked(b) => println("Clicked 1")
        case event => println(event)
    }


    def top = new MainFrame{
        title = "My First Swing App"
        contents = button1
        size = new Dimension(300, 80)
    }

    button1.peer.addActionListener(new ActionListener {
        def actionPerformed(e: ActionEvent) {
            println(e)
            println(List(button1.peer.getActionListeners))

        }
    })


}

//object SimpleSwing extends SimpleSwingApplication {
//    val button1 = new Button{
//        text = "Click me 1"
//    }
//    val button2 = new Button{
//        text = "Click me 2"
//    }
//
//    val reaction11:Reactions.Reaction = {
//        case ButtonClicked(b) => println("Clicked 1")
//        case _ =>
//    }
//
//    val reaction12:Reactions.Reaction = {
//        case ButtonClicked(b) => println("Another Clicked 1")
//        case _ =>
//    }
//
//    val reaction21:Reactions.Reaction = {
//        case ButtonClicked(b) => println("Clicked 2")
//            button1.unsubscribe(reaction12)
//        case _ =>
//    }
//
//    button1.subscribe(reaction11)
//    button1.subscribe(reaction12)
//    button2.subscribe(reaction21)
//
//    def top = new MainFrame{
//        title = "My First Swing App"
//        contents = new BoxPanel(Orientation.Vertical){
//            contents += button1
//            contents += button2
//        }
//    }
//}
