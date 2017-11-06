package example.papose

import cats.data.NonEmptyList

import example.emnlp2017.SentenceId
import turksem.util.ContiguousSpan

case class MissingPairRecord(
  sid: SentenceId,
  pairs: NonEmptyList[(ContiguousSpan, ContiguousSpan)]
)
