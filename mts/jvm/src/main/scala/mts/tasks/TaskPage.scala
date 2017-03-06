package mts.tasks

import scalatags.Text.all._
import upickle.default._

/** Contains the general HTML template for tasks. */
object TaskPage {
  def htmlPage[Prompt : Writer](prompt: Prompt, taskSpec: TaskSpecification, useHttps: Boolean = true)(implicit config: TaskConfig) = {
    import config._
    val protocol = if(useHttps) "https:" else "http:"
    val port = if(useHttps) httpsPort else httpPort
    html(
      head(
        script(
          `type` := "text/javascript",
          src := "https://s3.amazonaws.com/mturk-public/externalHIT_v1.js"
        ),
        script(
          `type` := "text/javascript",
          src := s"$protocol//$serverDomain:$port/mts-jsdeps.js"),
        script(
          `type` := "text/javascript",
          src := s"$protocol//$serverDomain:$port/mts-fastopt.js"),
        script(
          `type` := "text/javascript",
          src := s"$protocol//$serverDomain:$port/mts-launcher.js"),
        // TODO this is temporary while using copypasta'd stylesheets. get rid of this when all is unified grandly
        link(
          rel := "stylesheet",
          `type` := "text/css",
          href := s"$protocol//$serverDomain:$port/styles.css")
      ),
      body()(
        input(
          `type` := "hidden",
          value := write(prompt),
          name := promptLabel,
          id := promptLabel),
        input(
          `type` := "hidden",
          value := write(serverDomain),
          name := serverDomainLabel,
          id := serverDomainLabel),
        input(
          `type` := "hidden",
          value := write(httpPort),
          name := httpPortLabel,
          id := httpPortLabel),
        input(
          `type` := "hidden",
          value := write(httpsPort),
          name := httpsPortLabel,
          id := httpsPortLabel),
        input(
          `type` := "hidden",
          value := write(taskSpec.taskKey),
          name := taskKeyLabel,
          id := taskKeyLabel),
        form(
          name := mturkFormLabel,
          method := "post",
          id := mturkFormLabel,
          action := externalSubmitURL)(
          // where turk puts the assignment ID
          input(
            `type` := "hidden",
            value := "",
            name := "assignmentId",
            id := "assignmentId"),
          // where our client code should put the response
          input(
            `type` := "hidden",
            value := "",
            name := responseLabel,
            id := responseLabel),
          // and here I'll let the client code do its magic
          div(
            id := rootClientDivLabel,
            "Waiting for task data from server... (If this message does not disappear shortly, the server is down. Try refreshing in a minute or so.)"
          ),
          p(
            input(
              `type` := "text",
              name := feedbackLabel,
              placeholder := "Feedback? (Optional)",
              margin := 1,
              padding := 1,
              width := 484
            )
          ),
          input(
            `type` := "submit",
            disabled := true,
            id := submitButtonLabel,
            value := "submit")
        )
      )
    )
  }
}
