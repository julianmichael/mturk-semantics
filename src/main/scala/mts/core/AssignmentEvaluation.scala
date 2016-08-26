package mts.core

sealed trait AssignmentEvaluation
case class Approval(message: String) extends AssignmentEvaluation
case class Rejection(message: String) extends AssignmentEvaluation
