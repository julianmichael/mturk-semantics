package example.emnlp2017.silly

import turksem.util._

import cats.Id
import cats.Foldable
import cats.data.NonEmptyList
import cats.data.StateT
import cats.data.State
import cats.implicits._

import nlpdata.util.LowerCaseStrings._
import nlpdata.datasets.wiktionary._

import monocle.macros._

sealed trait Tense
case class Modal(modalVerb: LowerCaseString) extends Tense
case object PresentTense extends Tense
case object PastTense extends Tense

case class TenseAspect(
  tense: Tense,
  isPerfect: Boolean,
  isProgressive: Boolean,
  isNegated: Boolean)


object TenseAspect {

  def simplePast = TenseAspect(PastTense, false, false, false)

  class VerbRenderer(
    verbInflectedForms: InflectedForms,
    tenseAspect: TenseAspect,
    voice: VerbVoice) {
    import tenseAspect._
    val isPassive = voice == VerbVoice.Passive

    private[this] def modalTokens(modal: LowerCaseString) =
      if(isNegated) {
        if(modal.toString == "will") NonEmptyList.of("won't")
        else if(modal.toString == "can") NonEmptyList.of("can't")
        else if(modal.toString == "might") NonEmptyList.of("might", "not")
        else NonEmptyList.of(s"${modal}n't")
      } else {
        NonEmptyList.of(modal.toString)
      }

    private[this] def getForms(s: LowerCaseString) = {
      if(verbInflectedForms.allForms.contains(s)) Some(verbInflectedForms)
      else if(InflectedForms.beSingularForms.allForms.contains(s)) Some(InflectedForms.beSingularForms)
      else if(InflectedForms.doForms.allForms.contains(s)) Some(InflectedForms.doForms)
      else if(InflectedForms.haveForms.allForms.contains(s)) Some(InflectedForms.haveForms)
      else None
    }

    private[this] def push(s: String) =
      State.modify[NonEmptyList[String]](s :: _)
    private[this] def pushAll(ss: NonEmptyList[String]) =
      State.modify[NonEmptyList[String]](x => ss ++ x.toList)
    private[this] def modTop(f: String => String) =
      State.modify[NonEmptyList[String]](l => NonEmptyList(f(l.head), l.tail))
    private[this] def modForm(form: VerbForm) =
      modTop(w => getForms(w.lowerCase).fold(w)(_(form)))

    def getVerbTokens = {
      def pass = State.pure[NonEmptyList[String], Unit](())

      val stackState = for {
        // start with verb stem
        _ <- (if(isPassive) modForm(PastParticiple) >> push("be") else pass)
        _ <- (if(isPerfect) modForm(PastParticiple) >> push("have") else pass)
        _ <- (if(isProgressive) modForm(PresentParticiple) >> push("be") else pass)
        postAspectStack <- State.get[NonEmptyList[String]]
        _ <- tense match {
          case Modal(m) => pushAll(modalTokens(m))
          case PastTense => if(isNegated) {
            if(postAspectStack.size == 1) push("didn't")
            else modTop(_ + "n't")
          } else modForm(Past)
          case PresentTense => if(isNegated) {
            if(postAspectStack.size == 1) push("doesn't")
            else modTop(_ + "n't")
          } else modForm(PresentSingular3rd)
        }
      } yield ()

      stackState.runS(NonEmptyList.of(verbInflectedForms.stem)).value
    }

    // contractions here won't actually happen, just copied this here and lazy to delete them
    // because who knows, I might copy it again
    final val beVerbs = Set(
      "be", "being", "been",
      "am", "'m",
      "is", "'s", "ai",
      "are", "'re",
      "was", "were").map(_.lowerCase)

    def splitIfNecessary(verbStack: NonEmptyList[String]) = {
      if(verbStack.size > 1 || beVerbs.contains(verbStack.head.lowerCase)) {
        verbStack
      } else {
        verbInflectedForms.getForm(verbStack.head.lowerCase) match {
          case Some(form) => (modForm(Stem) >> push("do") >> modForm(form)).runS(verbStack).value
          case None => "does" :: verbStack // shouldn't happen, but whatever
        }
      }
    }

  }
}
