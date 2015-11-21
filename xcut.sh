#!/usr/bin/env bash

LOG=true

export WIDTH=600
export HEIGHT=600
export CUTS=5000
export SPEED=30

function rand {
  cat /dev/urandom \
    | env LC_CTYPE=C tr -dc 'a-zA-Z0-9' \
    | fold -w 32 \
    | head -n 1
}
export -f rand

function mkcut {
  PREF="__XCUT__$(rand)__"
  NAME="$(basename "$1")"
  IMG="$PREF$NAME"

  t0=$(date +%s)
  convert -depth 8 -resize ${WIDTH}x${HEIGHT} "$1" "$IMG.ppm"
  xcutT0=$(date +%s)
  ./Crosscut --path "$IMG.ppm" --ncuts $CUTS
  xcutT1=$(date +%s)

  rm "$IMG.ppm"
  convert $PREF*.ppm out.png
  rm $PREF*
  t1=$(date +%s)

  if $LOG; then
    echo "$NAME,cut,$WIDTH,$HEIGHT,$CUTS,$SPEED,$(expr $xcutT1 - $xcutT0),$(expr $t1 - $t0)" >> times.csv
  fi
}
export -f mkcut

function mkgif {
  PREF="__XCUT__$(rand)__"
  NAME="$(basename "$1")"
  IMG="$PREF$NAME"

  t0=$(date +%s)
  convert -depth 8 -resize ${WIDTH}x${HEIGHT} "$1" "$IMG.ppm"
  xcutT0=$(date +%s)
  ./Crosscut --anim --path "$IMG.ppm" --ncuts $CUTS
  xcutT1=$(date +%s)

  rm "$IMG.ppm"
  convert -delay $SPEED -loop 0 $PREF*.ppm "$NAME.gif"
  rm $PREF*
  t1=$(date +%s)

  if $LOG; then
    echo "$NAME,gif,$WIDTH,$HEIGHT,$CUTS,$SPEED,$(expr $xcutT1 - $xcutT0),$(expr $t1 - $t0)" >> times.csv
  fi
}
export -f mkgif

function ncores {
  if [ "$(uname)" = 'Darwin' ]; then
    sysctl -n hw.logicalcpu_max
  else
    lscpu -p | egrep -v '^#' | wc -l
  fi
}

J=$(expr $(ncores) - 1)

cmd=$1
shift
case $cmd in
  "--anim")
    parallel --will-cite --jobs $J mkgif ::: "$@"
    ;;
  *)
    parallel --will-cite --jobs $J mkcut ::: "$@"
    ;;
esac

