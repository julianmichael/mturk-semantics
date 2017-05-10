package turkey.experiments.sample

import turkey.tasks._

import scalajs.js.JSApp

object Dispatcher extends TaskDispatcher with JSApp {

  override val taskMapping = Map[String, () => Unit](
    sampleTaskKey -> (() => Client.main())
  )
}
