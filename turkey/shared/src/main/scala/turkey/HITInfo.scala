package turkey

case class HITInfo[Prompt, Response](
  hit: HIT[Prompt],
  assignments: List[Assignment[Response]])
