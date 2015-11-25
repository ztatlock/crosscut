#!/usr/bin/env bash

VIDEO="$1"

NAME=$(basename $1)
NAME=${NAME%.*}

SIZE=800
CUTS=500
RATE=40

function rand {
  cat /dev/urandom \
    | env LC_CTYPE=C tr -dc 'a-zA-Z0-9' \
    | fold -w 10 \
    | head -n 1
}

TMPDIR="/tmp"
PREFIX="${TMPDIR}/$(rand)-frame-"
CUTPAD=$(printf "%05d" ${CUTS})

echo "extracting frames"
ffmpeg -loglevel error -stats -y \
  -r ${RATE} -i "$VIDEO" \
  -r ${RATE} ${PREFIX}%05d.ppm

echo "xcutting"
parallel --progress \
  ./Crosscut \
    --maxDim2 ${SIZE} \
    --ncuts ${CUTS} \
    --outDir ${TMPDIR} \
    --path ::: ${PREFIX}*.ppm

echo "composing frames"
ffmpeg -loglevel error -stats -y \
  -r ${RATE} -i ${PREFIX}%*-xcut-${CUTPAD}.ppm \
  -r ${RATE} "${NAME}-xcut.mp4"

rm ${PREFIX}*.ppm
