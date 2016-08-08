# mturk-semantics

Natural language semantics of the people, by the people, for the people.

## Setup

After cloning this repo, run ```./setup.sh``` and it should tell you what to do
and take care of things for you.
If you don't have an easy URL (like a personal Dropbox link, or location on a remote server)
from which to download Ontonotes 5.0, download it yourself and place it in a folder
named ```ontonotes-release-5.0``` on the top level of the repo.

## Running

This project uses ```sbt``` for its build system.
The preferred way of running experiments is inside the console;
to jump right in you can run ```sbt console``` at the top level.
By default, experiments will run in the Mechanical Turk Requester Sandbox.
For an example experiment, just run ```OpenFormExperiment.start()``` on the console.
It will print out information about the HITs being created,
which you can view on the Mechanical Turk Requester Sandbox website.
To run in production, change the value of ```Config``` in
```mts.tasks.package.scala```, recompile, and run again.
