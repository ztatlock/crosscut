#!/usr/bin/env bash

SIZE=600
CUTS=500
RATE=30

CUTPAD=$(printf "%05d" $CUTS)

youtube-dl --recode-video mkv $1 -o video

ffmpeg -r $RATE -i video.mkv \
       -r $RATE video-%05d.ppm

parallel --progress --will-cite \
  ./Crosscut --maxDim2 $SIZE --ncuts $CUTS --path ::: video-*.ppm

ffmpeg -r $RATE -i video-%05d-xcut-$CUTPAD.ppm \
       -r $RATE video-xcut.mkv

rm video-*.ppm
