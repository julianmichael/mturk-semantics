package nlpdata.datasets.conll

import nlpdata.datasets.ptb._
import nlpdata.util._

import scala.util.{Try, Success, Failure}
import scala.collection.mutable
import scala.concurrent.duration._

import scala.language.implicitConversions
import scala.language.postfixOps

import java.nio.file.{Paths, Path, Files}

/** Provides logic for interfacing with the CoNLL-2012 data.
  * Includes data types and methods for reading instances from the data files.
  *
  * The main entry points are the FileManager extension methods,
  * used to read CoNLL files and sentences from paths.
  *
  * As of now, I don't have a programmatic store of all of the paths.
  * If you wish to load a CoNLL file, you will have to create the path yourself,
  * or find paths already in the code (for example, the sample paths in
  * [[mts.experiments]]).
  *
  * The package object contains extension methods for conveniently rendering text
  * from CoNLL data types and reading CoNLL data from the CoNLL-2012 files.
  */
trait PackagePlatformExtensions

