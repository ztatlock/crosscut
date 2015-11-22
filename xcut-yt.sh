#!/usr/bin/env bash

SIZE=600
CUTS=500
RATE=10

CUTPAD=$(printf "%05d" $CUTS)

youtube-dl --recode-video mkv $1 -o video

ffmpeg -i video.mkv -r $RATE video-%05d.ppm

parallel --progress --will-cite \
  ./Crosscut --maxDim $SIZE $SIZE --ncuts $CUTS --path ::: video-*.ppm

ffmpeg -i video-%05d-xcut-$CUTPAD.ppm -r $RATE video-xcut.mkv

rm video-*.ppm
