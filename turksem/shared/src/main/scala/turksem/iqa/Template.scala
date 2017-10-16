package turksem.iqa

import cats.Show
import cats.implicits._

import nlpdata.util.Text
import nlpdata.util.LowerCaseStrings._
import nlpdata.datasets.wiktionary.InflectedForms

case class Template(tokens: List[Token]) {
  def size = tokens.filter(_.isSlot).size
  def arguments = tokens.collect { case a: Argument => a }
  def numArguments = arguments.size
  def mainVerb = tokens.collect { case m: MainVerb => m }.head
  def gappableIndices = tokens.zipWithIndex.collect {
    case (_: GappableArgument, i) => i
  }
}
object Template {
  implicit val templateShow: Show[Template] = new Show[Template] {
    override def show(t: Template) = t.tokens.map(_.show).mkString(" ")
  }
}
