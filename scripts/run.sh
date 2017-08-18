#!/bin/bash

BASE=`dirname $0`/..
pushd $BASE
{ echo ":load scripts/init-$1.scala" & cat <&0; } | sbt "project $1JVM" console
popd
