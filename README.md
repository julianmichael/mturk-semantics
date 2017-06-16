# mturk-semantics

Natural language semantics of the people, by the people, for the people.

## About

This is a project for gathering semantic annotations from the crowd on Mechanical Turk (MTurk).
Details on the project coming soon.

### Dependencies

To use this project,
you need [Scala](http://www.scala-lang.org/) and [sbt](http://www.scala-sbt.org/).
The rest of the dependencies are managed and will be downloaded for you.
Some of the most important components include the following:
 * [nlpdata](http://www.github.com/julianmichael/nlpdata) has data types, NLP datasets, and text rendering.
 * [turkey](http://www.github.com/julianmichael/turkey) is used to manage and run jobs on Amazon Mechanical Turk.
 * [Stanford CoreNLP](http://stanfordnlp.github.io/CoreNLP/) is used for tokenization and POS-tagging.
 * [scalajs-react](https://github.com/japgolly/scalajs-react) is used for the Turk client interfaces.

## Setup

After cloning this repo, run ```./scripts/setup.sh``` (you need `wget`)
and it should tell you what to do and download the necessary files.

In order to run all of the examples, you will need to download the
[Penn Treebank](https://catalog.ldc.upenn.edu/ldc99t42) from the LDC---which requires a license---and
place it at `resources/ptb`.

In order to run any Mechanical Turk host, you will need to place a file called `mturk.properties`
in the base directory containing your AWS credentials for MTurk. (See `scripts/setup.sh`.)

## Usage

Assuming all of the setup is done, you can run our example annotation pipeline by executing `scripts/run.sh`.
This will open up the sbt console and start the webserver; at this point `exp.start` will begin uploading
HITs to the MTurk sandbox.
