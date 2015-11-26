#!/usr/bin/env bash

SCALE=2
NCUTS=1000
RATE=25
MINREG=25

IW=1280
IH=720
PIXFMT="rgb24"

OW="$(expr $IW '/' $SCALE)"
OH="$(expr $IH '/' $SCALE)"

function rand {
  cat /dev/urandom \
    | env LC_CTYPE=C tr -dc 'a-zA-Z0-9' \
    | fold -w 10 \
    | head -n 1
}

NONCE="$(rand)"
CAM="/tmp/xcut-${NONCE}-cam-fifo"
OUT="/tmp/xcut-${NONCE}-out-fifo"
mkfifo "${CAM}" "${OUT}"

ffmpeg \
  -y \
  -loglevel error \
  -r "${RATE}" \
  -f qtkit -i "default" \
  -f rawvideo \
  -pixel_format "${PIXFMT}" \
  -video_size "${IW}x${IH}" \
  -framerate "${RATE}" \
  -vf "scale=${OW}:${OH}" \
  "${CAM}" \
  &

./Crosscut \
  --rawVid "${CAM}" "${OUT}" \
  --ncuts "${NCUTS}" \
  --minReg "${MINREG}" \
  --maxDim "${OW}" "${OH}" \
  &

ffplay \
  -loglevel error \
  -f rawvideo \
  -pixel_format "${PIXFMT}" \
  -video_size "${OW}x${OH}" \
  -framerate "${RATE}" \
  -i "${OUT}"

rm "${CAM}" "${OUT}"
