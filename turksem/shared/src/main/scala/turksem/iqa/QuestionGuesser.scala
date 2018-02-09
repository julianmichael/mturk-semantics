// package turksem.iqa

// import turksem.util.ContiguousSpan

// import nlpdata.util.LowerCaseStrings._

// import cats.implicits._

// import monocle.macros.{Lenses, GenPrism}

// trait QuestionGuesser {
//   def guesses(state: QuestioningState): List[TemplatedQuestion]
//   def guess(state: QuestioningState): Option[TemplatedQuestion] = guesses(state).headOption
// }

// case class CountingQuestionGuesser(
// ) {
//   override def guesses(state: QuestioningState): List[TemplatedQuestion] = {
//     ???
//   }
// }

// object SimpleQuestionGuesser extends QuestionGuesser {

//   // use all prepositions as "external words"

//   val mostCommonPrepositions = Set(
//     "by", "for", "with", "in", "from", "to", "as"
//   ).map(_.lowerCase)

//   val lotsOfPrepositions = Set(
// 		"aboard", "about", "above", "across", "afore", "after", "against", "ahead", "along", "alongside", "amid",
// 		"amidst", "among", "amongst", "around", "as", "aside", "astride", "at", "atop", "before",
// 		"behind", "below", "beneath", "beside", "besides", "between", "beyond", "by", "despite", "down",
// 		"during", "except", "for", "from", "given", "in", "inside", "into", "near", "next",
// 		"of", "off", "on", "onto", "opposite", "out", "outside", "over", "pace", "per",
// 		"round", "since", "than", "through", "throughout", "till", "times", "to", "toward", "towards",
// 		"under", "underneath", "until", "unto", "up", "upon", "versus", "via", "with ", "within",
// 		"without"
//   ).map(_.lowerCase)

//   // simplest-ass guessing function I can come up with
//   // later we want to incorporate lots of statistics and knowledge into this
//   // right now only guessing trigger questions
//   final def guesses(state: QuestioningState): List[TemplatedQuestion] = {
//     // TODO add more prepositions when we can be smarter about it
//     val prepositions = state.sentence
//       .map(_.token.lowerCase)
//       .filter(lotsOfPrepositions.contains)
//       .toSet// ++ mostCommonPrepositions
//     state.triggerGroups.filterNot(_.trigger.pos == "IN").toList.flatMap { case TriggerGroup(trigger, templates, qas) =>
//       // None represents the answer
//       val candidateArguments = qas.collect {
//         case (qaIndex, qa) if qa.judgment.isAnswer =>
//           Some(qa.question.template.arguments(qa.question.targetIndex) -> qaIndex)
//       }.toVector :+ None
//       val questions = templates.flatMap { case (template, triggerIndex) =>
//         val existingInstantiations = qas.collect {
//           case (qaIndex, qa) if qa.question.template == template =>
//             qa.judgment.getAnswer.fold(qa.question.argsWithAnswerPlaceholder)(_ =>
//               qa.question.reconstructedTemplateArgs(Entity(qaIndex)).map(Option(_))
//             )
//         }
//         template.arguments.zipWithIndex.foldM[List, Vector[Option[ArgumentSpecifier]]](Vector.empty[Option[ArgumentSpecifier]]) {
//           case (as, (arg, i)) =>
//             val result = (as, (arg, i)) match {
//               case (args, (AlignedVerb(_), _)) => List(args :+ Some(SentenceWord(trigger.index))) // must be trigger
//               case (args, (Adjective(kind), argIndex)) if argIndex == triggerIndex =>
//                 List(args :+ Some(SentenceWord(trigger.index)))
//               case (args, (Adjective(kind), i)) =>
//                 // don't bother checking for existence in prev args bc we only allow one adjective anyway
//                 state.sentence
//                   .filter(_.pos == kind.pos)
//                   .map(adj => args :+ Some(SentenceWord(adj.index)))
//                   .toList
//               case (args, (Preposition, i)) =>
//                 prepositions
//                   .map(p => Some(ExternalWord(p)))
//                   .filterNot(args.contains)
//                   .map(args :+ _)
//                   .toList
//               case (args, (n @ Noun(_), i)) =>
//                 candidateArguments.collect {
//                   case Some((Noun(_), index)) => Some(Entity(index))
//                   case None => None } // assume DetOrVerb right now
//                   .filterNot(args.contains)
//                   .map(args :+ _)
//                   .toList
//               case (args, (ToVerb, _)) => candidateArguments.collect {
//                 case Some((ToVerb, index)) => Some(Entity(index))
//                 case None => None }
//                   .filterNot(args.contains)
//                   .map(args :+ _)
//                   .toList
//             }

//             if(trigger.index == 5 && template.toString == "Template(List(Noun(DetOrVerb), Copula, Adjective(Regular), Preposition, Noun(DetOrVerb)))") {
//               // println
//               // println("CHECKPOINT")
//               // println(s"Template: $template")
//               // println(s"Arguments: ${template.arguments}")
//               // println(s"Chosen args so far: $as")
//               // println(s"Next argument: $arg ($i)")
//               // println(s"Results:")
//               // println("==")
//               // result.foreach(println)
//               // println("==")
//             }
//             result
//         }.filter { newArgChoices =>
//           if(newArgChoices.size != template.arguments.size) {
//             // println("WARNING: trying to generate bad argument list")
//             // println("Template: " + template)
//             // println("Index of trigger in template: " + triggerIndex)
//             // println("Arguments: " + template.arguments)
//             // println("Argument choices: " + newArgChoices)
//           }
//           newArgChoices.size == template.arguments.size &&
//             newArgChoices.contains(None) && !existingInstantiations.exists { prevArgChoices =>
//             val alignments = (prevArgChoices, newArgChoices).zipped.collect {
//               case (Some(x), Some(y)) => x == y
//             }
//             // TODO not sure if this is best
//             alignments.size > 0 && alignments.forall(identity)
//           }
//         }.map { args =>
//           // println("FINAL:")
//           // println("template: " + template.show)
//           // println("args: " + args)
//           // println
//           val targetIndex = args.indexOf(None)
//           TemplatedQuestion(
//             template, targetIndex,
//             TenseAspect.simplePast.copy(tense = PresentTense),
//             args.flatten.toList
//           )
//         }
//       }
//       questions
//     }.sortBy(_.arguments.size)
//   }
// }
