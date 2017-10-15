import example.emnlp2017.silly._
import cats.implicits._

val t = templates
val ta = TenseAspect.simplePast.copy(tense = PresentTense)

def renderQuestionsWithNumArguments(i: Int) = {
  t.all.filter(_.numArguments == i).map(t => (t.show :: (0 until i).filter(t.gappableIndices.contains).map(t.testInstantiate(ta, _)).toList).map(x => f"$x%-25s").mkString(" ")).foreach(println)
}
