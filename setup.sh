#!/bin/bash

# Setup script for coref project.
# Written by Julian Michael on 13 June 2016.
# Downloads OntoNotes 5.0 and CoNLL 2012 data,
# and runs the CoNLL script to format the data so it's ready to be parsed.

# NOTE: the CoNLL 2012 task instructions say to use the CoNLL 2012 package from the LDC.
# I used the OntoNotes 5.0 package instead and it seems to work fine.
# It's the same data, and the directory structure seems to be the same too.

if [ ! -e "ontonotes-url.txt" ]
then
  echo "Missing file: ontonotes-url.txt"
  echo "Please add this file, consisting only of a download URL for the OntoNotes 5.0 release."
  echo "This URL may not be included in the repository because the data is not publicly available."
  echo "LDC page: https://catalog.ldc.upenn.edu/LDC2013T19"
  exit 0
fi


if [ ! -e "ontonotes-release-5.0/" ]
then
  read -p $'Download the OntoNotes 5.0 release? [y/N]\n' answer
  case ${answer:0:1} in
    y|Y )
      wget \
        --no-check-cert `cat ontonotes-url.txt` \
        -O ontonotes-release-5.0.tar.gz
      tar \
        -xvzf ontonotes-release-5.0.tar.gz
      rm ontonotes-release-5.0.tar.gz
    ;;
    * )
      echo "Terminating. Get the data yourself."
      exit 0
    ;;
esac
fi

if [ ! -e "conll-2012/" ]
then
  read -p $'Download the CoNLL 2012 data? [y/N]\n' answer
  case ${answer:0:1} in
    y|Y )
      wget \
        --no-check-cert https://www.dropbox.com/s/ju8yvet1qx25ilt/conll-2012-development.v4.tar.gz?dl=1 \
        -O conll-2012-development.v4.tar.gz
      wget \
        --no-check-cert https://www.dropbox.com/s/si4ta4viey9u1s3/conll-2012-train.v4.tar.gz?dl=1 \
        -O conll-2012-train.v4.tar.gz
      wget \
        --no-check-cert https://www.dropbox.com/s/h2dvy4zz62nq4ft/conll-2012-scripts.v3.tar.gz?dl=1 \
        -O conll-2012-scripts.v3.tar.gz
      tar zxvf conll-2012-train.v4.tar.gz
      tar zxvf conll-2012-development.v4.tar.gz
      tar zxvf conll-2012-scripts.v3.tar.gz
      rm conll-2012-train.v4.tar.gz
      rm zxvf conll-2012-development.v4.tar.gz
      rm zxvf conll-2012-scripts.v3.tar.gz
    ;;
    * )
      echo "Terminating. Get the data yourself."
      exit 0
    ;;
esac
fi

# This is the last file that should be produced by the above script
if [ ! -e "conll-2012/v4/data/development/data/chinese/annotations/wb/e2c/00/e2c_0010.v4_gold_conll" ]
then
  read -p $'It seems that the _conll files have not been generated yet. Do it now? [y/N]\n' answer
  case ${answer:0:1} in
    y|Y )
      conll-2012/v3/scripts/skeleton2conll.sh -D \
        ontonotes-release-5.0/data/files/data/ \
        conll-2012/
    ;;
    * )
      echo "Terminating. Do it yourself."
      exit 0
    ;;
esac
fi

