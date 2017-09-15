package turksem.util

import cats.Foldable
import cats.implicits._

// TODO not actually using any of these yet lol
// TODO might need to add parametrization by aligners or something instead of raw sets,
// also have the issue of wanting to align things only once? maybe?

trait SetMetric {
  def apply[F[_]: Foldable, A](space: F[A])(predict: A => Boolean, reference: A => Boolean): Double
  final def apply[A](predict: Set[A], reference: Set[A]): Double = {
    apply(predict union reference)(predict, reference)
  }
}

/** Utility functions for judging similarity of sets, sets of sets, etc. */
object SetMetric {

  val accuracy = new SetMetric {
    override def apply[F[_]: Foldable, A](space: F[A])(predict: A => Boolean, reference: A => Boolean) = {
      var numCorrect = 0
      var numTotal = 0
      space.foldMap { a =>
        if(predict(a) == reference(a)) numCorrect += 1
        numTotal += 1
      }
      numCorrect.toDouble / numTotal
    }
  }

  val precision = new SetMetric {
    override def apply[F[_]: Foldable, A](space: F[A])(predict: A => Boolean, reference: A => Boolean) = {
      var numCorrect = 0
      var numGuessed = 0
      space.foldMap { a =>
        if(predict(a) && reference(a)) numCorrect += 1
        if(predict(a)) numGuessed += 1
      }
      numCorrect.toDouble / numGuessed
    }
  }

  val recall = new SetMetric {
    override def apply[F[_]: Foldable, A](space: F[A])(predict: A => Boolean, reference: A => Boolean) = {
      var numDetected = 0
      var numPresent = 0
      space.foldMap { a =>
        if(predict(a) && reference(a)) numDetected += 1
        if(reference(a)) numPresent += 1
      }
      numDetected.toDouble / numPresent
    }
  }

  val f1 = new SetMetric {
    override def apply[F[_]: Foldable, A](space: F[A])(predict: A => Boolean, reference: A => Boolean) = {
      val prec = precision(space)(predict, reference)
      val rec = recall(space)(predict, reference)
      2 * (prec + rec) / (prec + rec)
    }
  }

  val intersectionOverUnion = new SetMetric {
    override def apply[F[_]: Foldable, A](space: F[A])(predict: A => Boolean, reference: A => Boolean) = {
      var numInBoth = 0
      var numInEither = 0
      space.foldMap { a =>
        if(predict(a) && reference(a)) numInBoth += 1
        if(predict(a) || reference(a)) numInEither += 1
      }
      numInBoth.toDouble / numInEither
    }
  }
}
