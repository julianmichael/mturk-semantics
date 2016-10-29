# mturk-semantics

Natural language semantics of the people, by the people, for the people.

## About

This is a project for gathering semantic annotations from the crowd on Mechanical Turk (MTurk).
However, the framework here may be useful to anyone who wants to experiment with crowdsourcing.
We've taken care of most of the details of the MTurk API so you can spend more time designing your task.

### Dependencies

To use this project,
you need [Scala](http://www.scala-lang.org/) and [sbt](http://www.scala-sbt.org/).
The rest of the dependencies are managed and will be downloaded for you.
This includes the following:

 * [The Amazon Mechanical Turk SDK for Java](https://requester.mturk.com/developer/tools/java)
   is used to interface with the MTurk API.
 * [Akka](http://akka.io/) actors are used for asynchronous task management.
 * [Stanford CoreNLP](http://stanfordnlp.github.io/CoreNLP/)
   is used for simple NLP tasks like tokenization.
 * [upickle](https://github.com/lihaoyi/upickle-pprint)
   is used for data serialization.
 * [Scalatags](https://github.com/lihaoyi/scalatags)
   is used for generating HTML for the uploaded HITs.
   
Other useful but less prominent dependencies include
[macmemo](https://github.com/kciesielski/macmemo) for function memoization,
[fastparse](https://github.com/lihaoyi/fastparse) for parsing data files,
[scala-arm](https://github.com/jsuereth/scala-arm) for automatic resource management, and
[scalaz](https://github.com/scalaz/scalaz) for (simple) functional programming.
This project uses [scala.js](https://www.scala-js.org/) to generate the client-side JavaScript
from Scala code, allowing you to share your data structures between the frontend and backend.

## Setup

After cloning this repo, run ```./setup.sh``` and it should tell you what to do
and download the necessary files.
If you already have the CoNLL 2012 data,
you may place or symlink it at ```resources/conll-2012``` to skip the download step.
You will also need it to be augmented with the ```_conll``` files
which contain the actual words from Ontonotes.
If this is not done in your CoNLL data,
you will need to download [Ontonotes release 5.0](https://catalog.ldc.upenn.edu/LDC2013T19),
which requires a license from the [Linguistic Data Consortium](https://www.ldc.upenn.edu/).
If you have URL (like a personal Dropbox link, or location on a remote server)
from which to download this, place it in a file called ```ontonotes-url.txt```
on the top level of the repo and the setup script will take care of things for you.
Otherwise you may place or symlink the data at ```resources/ontonotes-release-5.0```.

## Usage

The preferred way of running experiments is inside the sbt console.
to jump right in, run ```sbt ";project mtsJVM ;console"``` at the top level of the repo.
For an example experiment, make sure ```mts.tasks.Config``` is set to ```SandboxTaskConfig```
and run ```mts.experiments.expD.WordChoosingExperiment.start``` on the ```sbt``` console.
It will print out information about the HITs being created,
which you can view on the Mechanical Turk Requester Sandbox website.

For interacting with an experiment on the console, an ```Experiment``` object like ```WordChoosingExperiment```
provides a few methods:

  * ```update``` reviews and saves new annotations, uploading new HITs to replace finished ones.
  * ```start``` tells an experiment to ```update``` at regular intervals.
  * ```stop``` stops the continual updates.
  * ```disable``` reviews any completed assignments and removes all HITs currently online on MTurk.

It is generally easiest to just import these methods on the console with, e.g.,
```import mts.experiments.expD.WordChoosingExperiment._```.
You may interact with the results live on the console, retrieving them with, e.g.,
```mts.util.FileManager.loadAllData[Prompt, Response](hitType)```,
where ```Prompt```, ```Response```, and ```hitType``` will be specific to the experiment.
See the documentation of package [tasks/package.scala](mts/jvm/src/main/scala/mts/tasks/package.scala) for details,
and see the subpackages of the [experiments](mts/jvm/src/main/scala/mts/experiments/) package for example experiments.

## Overview

There isn't too much code here, but it's broken into several packages.
The purpose of each is documented in its package.scala file.
Here is a brief road map:

 * [conll](mts/jvm/src/main/scala/mts/conll/) contains everything related to interfacing with and interpreting the CoNLL data.
 * [core](mts/jvm/src/main/scala/mts/core/) contains a few core abstractions for working with HITs on MTurk.
 * [experiments](mts/jvm/src/main/scala/mts/experiments/) is for subpackages specifying actual experiments to run on MTurk.
 * [language](mts/jvm/src/main/scala/mts/language/) is for utilities specific to processing natural language input.
 * [tasks](mts/jvm/src/main/scala/mts/tasks/) is for the core abstractions used to interact with the MTurk API and manage ongoing experiments.
 * [util](mts/jvm/src/main/scala/mts/util/) is for miscellaneous, non-specific helper functions and data structures.
 * [validation](mts/jvm/src/main/scala/mts/validation/) is for classes that provide ways to validate worker responses.
 
The most relevant ones for the main ideas are core, tasks, and experiments.
Most everything outside of the experiments package can be considered "framework" and is well-documented.
The experiments are examples of how to use the framework.

## Making your own experiment

So you want to try something out and run it on MTurk.
Very briefly, here are the steps you should take:

 * Set up the repo. If you have everything, ```./setup.sh``` should not prompt you for anything.
 * Define a ```Prompt``` type and ```Response``` type.
   These are essentially the input and output types of your annotation task.
   The prompt is the information that determines which question you show to annotators,
   and the response is the information that you extract from their annotation.
   A prompt may be, for example, an identifier for a sentence in the CoNLL data,
   which is the one you will show to an annotator for a single HIT.
   A response may be, for example, a list of pairs of strings representing question/answer pairs.
   Since they are just data types, these should be defined as ```case class```es.
   This will also make them automatically serializable by ```upickle```.
   See [expD/package.scala](mts/jvm/src/main/scala/mts/experiments/expD/package.scala) for an example.
 * Write the ```TaskSpecification``` for your task.
   This is where you define the transformation from your ```Prompt```s into questions to upload as HITs
   (including the task instructions),
   and the transformation from an annotator's response to your ``Response``.
   This is also where you specify the general description for your task and other metadata like the payment.
   See [TaskSpecification.scala](mts/jvm/src/main/scala/mts/tasks/TaskSpecification.scala) for documentation,
   and [WordChoosingTask.scala](mts/jvm/src/main/scala/mts/experiments/expD/WordChoosingTask.scala) for an example.
 * Write or choose a ```DataManager``` for your task.
   This is where you define how the task avoids redundancy between restarts and decides which prompts to upload.
   See [DataManager.scala](mts/jvm/src/main/scala/mts/tasks/DataManager.scala) for documentation
   and [WordChoosingDataManager.scala](mts/jvm/src/main/scala/mts/experiments/expD/WordChoosingDataManager.scala) for an example.
 * Write the ```Experiment``` object for your task.
   As of now there is no ```Experiment``` trait to inherit from,
   but this is the general pattern I've used that has worked well.
   You just need a single place to tie the task specification and data manager together with a ```TaskMonitor```,
   decide which data you wish to have annotated,
   and provide convenience methods of your choice for interacting on the console.
   See [TabooAnswersExperiment.scala](mts/jvm/src/main/scala/mts/experiments/expC/TabooAnswersExperiment.scala) or
   [WordChoosingExperiment.scala](mts/jvm/src/main/scala/mts/experiments/expD/WordChoosingExperiment.scala) for examples.

After this you should be ready to go!

