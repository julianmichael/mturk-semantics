package mts.core

case class HITInfo[Prompt, Response](
  hit: HIT[Prompt],
  assignments: List[Assignment[Response]])
