package mts.experiments.sample

import mts.experiments._
import mts.conll._

import scalajs.js
import org.scalajs.dom
import org.scalajs.dom.raw._
import org.scalajs.jquery.jQuery

import scala.concurrent.ExecutionContext.Implicits.global

import autowire._
import upickle.default._

object Client extends TaskClient[SamplePrompt, SampleResponse] {
  def main(): Unit = jQuery { () =>
    println(assignmentId)
    println(prompt)
    println(externalSubmitURL)

    val socket = new dom.WebSocket(websocketUri)
    socket.onopen = { (event: Event) =>
      println("Websocket opened.")
    }
    socket.onerror = { (event: ErrorEvent) =>
      val msg = s"Failed to connect. Error code: ${event.colno}"
      System.err.println(msg)
    }
    socket.onmessage = { (event: MessageEvent) ⇒
      val response = read[ApiResponse](event.data.toString)
      response match {
        case SentenceResponse(path, sentence) =>
          println(sentence)
      }
    }
    socket.onclose = { (event: Event) =>
      val msg = s"Connection lost."
      System.err.println(msg)
    }

    socket.send(write(SentenceRequest(prompt.path)))

    setResponse(SampleResponse(true))
  }

  // QA specification methods

  // import scalatags.Text.all._
  // final override def instructions(prompt: SamplePrompt) = div(
  //   h2("""Task Summary"""),
  //   p("""This is a sample task. Please indicate whether the given sentence is good.
  //         Examples of good sentences include:"""),
  //   ul(
  //     li("""Why did you vote for Hillary when you knew she would send me to war?"""),
  //     li("""Make America great again."""),
  //     li("""Tell her that a double-income family is actually the true Igbo tradition because in pre-colonial times, mothers farmed and traded."""),
  //     li("""Chudi does not deserve any special gratitude or praise, nor do you ---
  //            you both made the choice to bring a child into the world, and the responsibility for that child belongs equally to you both.""")),
  //   p("""Examples of not-good sentences include:"""),
  //   ul(
  //     li("""So because of her unfounded concern over vote rigging, she committed voter fraud."""),
  //     li("""Comey told FBI employees he didn't want to "be misleading to the American people" by not supplementing the record of the investigation."""),
  //     li("""Donald Trump and Vladimir Putin's bromance has been the weirdest subplot of America's wild presidential election.""")),
  //   hr(),
  //   p(s"""Please indicate whether the following sentence is good:""")
  // )

  // final override def makeJSPrompt(prompt: SamplePrompt): String = {
  //   val sentence = FileManager.getCoNLLSentence(prompt.path).toOptionPrinting.get
  //   TextRendering.renderSentence(sentence)
  // }

  // private[this] final val checkboxName = "isGood"
  // private[this] final val checkbox = {
  //   import scalatags.Text.all._
  //   p(
  //     margin := 0,
  //     padding := 0
  //   )(
  //     input(
  //       `type` := "checkbox",
  //       name := checkboxName,
  //       font := pageFont
  //     ),
  //     label(
  //       `for` := checkboxName,
  //       "Yes, it is a good sentence."
  //     )
  //   )
  // }

  }
