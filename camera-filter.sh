#!/usr/bin/env bash

PIXFMT="rgb24"
DIM="1280x720"
RATE="20"

ffmpeg \
  -r "${RATE}" \
  -f qtkit -i "default" \
  -f rawvideo \
  -pixel_format "${PIXFMT}" \
  -video_size "${DIM}" \
  -framerate "${RATE}" \
  - | \
ffplay \
  -f rawvideo \
  -pixel_format "${PIXFMT}" \
  -video_size "${DIM}" \
  -framerate "${RATE}" \
  -i -
