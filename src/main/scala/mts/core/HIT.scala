package mts.core

case class HIT[Prompt](
  hitType: String,
  hitId: String,
  prompt: Prompt,
  creationTime: Long)
