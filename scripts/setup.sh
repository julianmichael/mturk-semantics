#!/bin/bash

# Setup script for MTurk Semantics project.
# This script is idempotent; don't be afraid to run it multiple times.
# Downloads all of the requisite data except the Penn Treebank.
# Also makes sure a file with your Mechanical Turk access keys is present.

# NOTE: If you already have any of the datasets somewhere, go just symlink
# resources/<dirname> to it (for whatever dirname it expects in this script)
# so it doesn't have to be downloaded again.

BASE=`dirname $0`/..
pushd $BASE

# initialize submodules
git submodule update --init --recursive
# publish local dependencies
echo "Publishing nlpdata locally..."
pushd lib/nlpdata
sbt publishLocal
popd
echo "Publishing spacro locally..."
pushd lib/spacro
sbt publishLocal
popd
echo "Publishing qamr locally..."
pushd lib/qamr/code
sbt publishLocal
popd
echo "Publishing qasrl-crowdsourcing locally..."
pushd lib/qasrl-crowdsourcing
sbt publishLocal
popd

# TODO check whether a domain is certified for HTTPS
# if [ ! -e $BASE/mts/jvm/src/main/resources/*.p12 ] # NOTE unix glob does NOT work here
# then
#   echo "-WARNING- In order to use HTTPS, please put a .p12 keystore and password in the directory:"
#   echo "mts/jvm/src/main/resources"
# fi

# if [ ! -e "resources/ptb/" ]
# then
#     echo "-WARNING- Please download the Penn Treebank and place it at resources/ptb in order to run experiments related to the Penn Treebank data (including those with PropBank and NomBank). Relevant projects: emnlp2017"
#     echo "It requires an LDC license. Webpage: https://catalog.ldc.upenn.edu/ldc99t42"
#     # TODO did I use the PTB 3?
# fi

if [ ! -e "resources/nombank.1.0/" ]
then
    read -p $'Download NomBank? (relevant projects: emnlp2017) [y/N]\n' answer
    case ${answer:0:1} in
        y|Y )
            wget http://nlp.cs.nyu.edu/meyers/nombank/nombank.1.0.tgz \
                 -O nombank.1.0.tgz
            tar zxvf nombank.1.0.tgz
            rm nombank.1.0.tgz
            mv nombank.1.0 resources/nombank.1.0
            ;;
        * )
            echo "Skipping NomBank. Run setup.sh again if you change your mind."
            ;;
    esac
fi

if [ ! -e "resources/qasrl/" ]
then
    read -p $'Download the QA-SRL data? (relevant projects: emnlp2017) [y/N]\n' answer
    case ${answer:0:1} in
        y|Y )
            wget https://www.dropbox.com/s/dvfk6rhiuzc5rmw/qasrl.tar.gz?dl=1 \
                 -O qasrl.tar.gz
            tar zxvf qasrl.tar.gz
            rm qasrl.tar.gz
            mv qasrl resources/qasrl
            ;;
        * )
            echo "Skipping QA-SRL. Run setup.sh again if you change your mind."
            ;;
    esac
fi

if [ ! -e "resources/wiki1k/" ]
then
    read -p $'Download Wiki1k data? (relevant projects: emnlp2017) [y/N]\n' answer
    case ${answer:0:1} in
        y|Y )
            wget https://www.dropbox.com/s/j5rmbppa4mk6hit/wiki1k.tar.gz?dl=1 \
                 -O wiki1k.tar.gz
            tar zxvf wiki1k.tar.gz
            rm wiki1k.tar.gz
            mv wiki1k resources/wiki1k
            ;;
        * )
            echo "Skipping Wiki1k. Run setup.sh again if you change your mind."
            ;;
    esac
fi

if [ ! -e "resources/wiktionary/" ]
then
    read -p $'Download the Wiktionary data? (relevant projects: all) [y/N]\n' answer
    case ${answer:0:1} in
        y|Y )
            wget \
                --no-check-cert https://www.dropbox.com/s/60hbl3py7g3tx12/wiktionary.tar.gz?dl=1 \
                -O wiktionary.tar.gz
            tar zxvf wiktionary.tar.gz
            rm wiktionary.tar.gz
            mv wiktionary resources/wiktionary
            ;;
        * )
            echo "Skipping Wiktionary. Run setup.sh again if you change your mind."
            ;;
    esac
fi

popd
