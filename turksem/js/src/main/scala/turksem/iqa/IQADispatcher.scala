// package turksem.iqa

// import spacro.tasks._

// import scalajs.js
// import scalajs.js.JSApp
// import org.scalajs.jquery.jQuery

// import japgolly.scalajs.react.vdom.html_<^._

// import upickle.default._

// import scalajs.js.JSApp

// abstract class IQADispatcher[SID : Reader : Writer] extends TaskDispatcher {

//   def instructions: VdomTag

//   lazy val client = new IQAClient[SID](instructions)

//   final override lazy val taskMapping = Map[String, () => Unit](
//     iqaTaskKey -> client.main)
// }
