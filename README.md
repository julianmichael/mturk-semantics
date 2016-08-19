# mturk-semantics

Natural language semantics of the people, by the people, for the people.

## Setup

After cloning this repo, run ```./setup.sh``` and it should tell you what to do
and download the necessary files.
Full setup requires the CoNLL 2012 data augmented with ```_conll``` files.
If you already have this, you may place or symlink it at ```resources/conll-2012```.
Otherwise, you will need to download [Ontonotes release 5.0](https://catalog.ldc.upenn.edu/LDC2013T19),
which requires a license from the [Linguistic Data Consortium](https://www.ldc.upenn.edu/).
If you have URL (like a personal Dropbox link, or location on a remote server)
from which to download this, place it in a file called ```ontonotes-url.txt```
on the top level of the repo and the setup script will take care of things for you.
Otherwise you may place or symlink it at ```resources/ontonotes-release-5.0```.

## Running

This project uses ```sbt``` for its build system.
The preferred way of running experiments is inside the console;
to jump right in you can run ```sbt console``` at the top level.
For an example experiment, make sure ```mts.tasks.Config``` is set to ```SandboxTaskConfig```
and run ```mts.experiments.expA.OpenFormExperiment.start``` on the sbt console.
It will print out information about the HITs being created,
which you can view on the Mechanical Turk Requester Sandbox website.
