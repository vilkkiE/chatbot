package chatbot

import scala.swing._
import javax.swing.ImageIcon
import java.awt.Color
import javax.swing.UIManager
import scala.swing.event._
import scala.io.StdIn._
import java.awt.Font
import java.io.PrintWriter
import javax.swing.JPopupMenu

object BotGUI extends SimpleSwingApplication {

  val chatBot = new Bot

  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)

  val pääIkkuna = new MainFrame

  def top = this.pääIkkuna

  val height = 850
  val width = 800

  this.pääIkkuna.title = "Chatbot"
  this.pääIkkuna.minimumSize = new Dimension(400, 600)
  this.pääIkkuna.preferredSize = new Dimension(width, height)
  this.pääIkkuna.resizable = true
  this.pääIkkuna.maximumSize = new Dimension(850, 950)
  this.pääIkkuna.peer.setLocationRelativeTo(null)

  val pääPaneeli = new BorderPanel

  pääIkkuna.contents = pääPaneeli

  pääPaneeli.background = new Color(240, 240, 240)

  // Tällä metodilla voi tallentaa dialogin haluamaansa paikkaan tietokoneella. Nimen perään
  // pitää kuitenkin laittaa ".txt"
  def save = {
    val organizedText = output.text.split("\n")
    val chooser = new FileChooser
    if (chooser.showSaveDialog(null) == FileChooser.Result.Approve) {
      val file = new PrintWriter(chooser.selectedFile)
      try {
        for (line <- organizedText) {
          file.println(line)
        }
      } finally {
        file.close()
      }
    }
  }

  top.menuBar = new MenuBar {
    contents += new Menu("Program") {
      val quitAction = Action("Quit") { top.dispose() }
      val saveAction = Action("Save") { save }
      contents += new MenuItem(saveAction)
      contents += new MenuItem(quitAction)
    }
  }

  val label = new Label {
    val image = this.getClass().getClassLoader().getResource("talking.jpg")
    icon = new ImageIcon(image)
  }

  pääPaneeli.layout(label) = BorderPanel.Position.North

  val input = new TextField("Type your message here..", 50) {
    minimumSize = preferredSize
  }

  val sendButton = new Button("Send") {
    preferredSize = new Dimension(120, 80)
    font = new Font("Arial", 0, 18)
    background = new Color(200, 200, 200)
  }

  this.listenTo(sendButton)

  val inputPanel = new FlowPanel() {
    background = new Color(240, 240, 240)
    contents += input
    contents += sendButton
  }

  val textPanel = new BoxPanel(Orientation.Vertical)

  pääPaneeli.layout(textPanel) = BorderPanel.Position.Center

  this.listenTo(input.keys)
  this.listenTo(input.mouse.clicks)

  // "Lähettää" käyttäjän inputin. Toisin sanoen vaihtaa edellisen inputin, etsii vastauksen
  // ja näyttää sen. Samalla myös tyhjentää input textfieldin.
  def sendMessage = {
    val newInput = this.input.text
    if (newInput.nonEmpty) {
      this.input.text = ""
      this.chatBot.updateUserInput(newInput)
      if (output.text.isEmpty) {
        output.text = "Me: " + newInput
        output.text += "\nChatbot: " + chatBot.findResponse
      } else {
        output.text = output.text + "\nMe: " + newInput
        output.text += "\nChatbot: " + chatBot.findResponse
      }
    }
  }

  this.reactions += {
    case keyEvent: KeyPressed =>
      if (keyEvent.source == this.input && keyEvent.key == Key.Enter) sendMessage
    case mouseEvent: MouseClicked =>
      if (mouseEvent.source == this.input) {
        input.text = ""
        this.deafTo(input.mouse.clicks)
      }
    case buttonClick: ButtonClicked => sendMessage

  }

  val output = new TextArea(12, 50) {
    editable = false
    wordWrap = true
    lineWrap = true
    font = new Font("Verdana", 0, 16)
  }

  val outputScroll = new ScrollPane(output)

  textPanel.contents += outputScroll
  textPanel.contents += inputPanel

}
  