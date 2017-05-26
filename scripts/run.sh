#!/bin/bash

{ echo ":load scripts/init.scala" & cat <&0; } | sbt "project mtsJVM" console
