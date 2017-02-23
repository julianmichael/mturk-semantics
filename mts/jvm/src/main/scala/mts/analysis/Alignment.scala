package mts.analysis

import mts.util.LowerCaseStrings._
import mts.language.Inflections

abstract class AlignmentValidator(infls: Inflections) {
  def alignedIndices(reference: List[LowerCaseString], queryWord: LowerCaseString): Set[Int] = {
    reference.zipWithIndex.filter(p => infls.getAllForms(p._1).contains(queryWord)).map(_._2).toSet
  }
  def alignedIndices(reference: List[LowerCaseString], query: List[LowerCaseString]): Set[Int] = {
    val querySet = query.toSet
    reference.zipWithIndex.filter(p => infls.getAllForms(p._1).exists(querySet.contains)).map(_._2).toSet
  }
  def isValid(reference: List[LowerCaseString], query: List[LowerCaseString]): Boolean
}

class SomeWordValidator(infls: Inflections) extends AlignmentValidator(infls) {
  override def isValid(reference: List[LowerCaseString], query: List[LowerCaseString]): Boolean =
    !alignedIndices(reference, query).isEmpty
}

class AllWordsValidator(infls: Inflections) extends AlignmentValidator(infls) {
  override def isValid(reference: List[LowerCaseString], query: List[LowerCaseString]): Boolean =
    query.forall(w => !alignedIndices(reference, w).isEmpty)
}
