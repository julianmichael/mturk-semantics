#!/bin/bash

# Setup script for MTurk Semantics project.
# Downloads Wiktionary, OntoNotes 5.0 and CoNLL 2012 data,
# and runs the CoNLL script to format the data so it's ready to be parsed.
# Also makes sure a file with your Mechanical Turk access keys is present.

# NOTE: the CoNLL 2012 task instructions say to use the CoNLL 2012 package from the LDC.
# I used the OntoNotes 5.0 package instead and it seems to work fine.
# It's the same data, and the directory structure seems to be the same too.

# NOTE: If you already have this data somewhere, go ahead and just symlink
# resources/<dirname> to it (for whatever dirname it expects in this script)
# and it will accommodate you.

if [ ! -e "mturk.properties" ]
then
    echo "Missing file: mturk.properties"
    echo "Please add a file named `mturk.properties`, consisting of two lines, of the form"
    echo "access_key=XXXXXXXXXXXXXXXXXXXX"
    echo "secret_key=YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY"
    echo "where the keys are obtained from your AWS account."
fi

if [ ! -e "resources/wiktionary/" ]
then
    read -p $'Download the Wiktionary data? [y/N]\n' answer
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
            echo "Skipping Wiktionary. Run `setup.sh` again if you change your mind."
            ;;
    esac
fi

if [ ! -e "resources/conll-2012/" ]
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
            mv conll-2012 resources/conll-2012
            ;;
        * )
            echo "Terminating. Get the data yourself."
            exit 0
            ;;
    esac
fi

# This is the last file that should be produced by the conll generation script
if [ ! -e "resources/conll-2012/v4/data/development/data/chinese/annotations/wb/e2c/00/e2c_0010.v4_gold_conll" ]
then
    if [ ! -e "resources/ontonotes-release-5.0/" ]
    then
        if [ ! -e "ontonotes-url.txt" ]
        then
            echo "Missing file: ontonotes-url.txt"
            echo "Please add a file named `ontonotes-url.txt`, consisting only of a download URL for the OntoNotes 5.0 release (as a .tar.gz), or place a copy or symlink of the OntoNotes 5.0 release in a folder named `ontonotes-release-5.0`. A download URL cannot be included in this repository because the data is not publicly available."
            echo "LDC page: https://catalog.ldc.upenn.edu/LDC2013T19"
            exit 0
        fi

        read -p $'Download the OntoNotes 5.0 release? [y/N]\n' answer
        case ${answer:0:1} in
            y|Y )
                wget \
                    --no-check-cert `cat ontonotes-url.txt` \
                    -O ontonotes-release-5.0.tar.gz
                tar \
                    -xvzf ontonotes-release-5.0.tar.gz
                rm ontonotes-release-5.0.tar.gz
                mv ontonotes-release-5.0.tar.gz resources/ontonotes-release-5.0.tar.gz
                ;;
            * )
                echo "Terminating. Get the data yourself."
                exit 0
                ;;
        esac
    fi

    read -p $'It seems that the _conll files have not been generated yet. Do it now? [y/N]\n' answer
    case ${answer:0:1} in
        y|Y )
            resources/conll-2012/v3/scripts/skeleton2conll.sh -D \
                resources/ontonotes-release-5.0/data/files/data/ \
                resources/conll-2012/
            ;;
        * )
            echo "Terminating. Do it yourself."
            exit 0
            ;;
    esac
fi

# TODO: download all of the other datasets too...
