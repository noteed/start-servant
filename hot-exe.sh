#! /usr/bin/env bash

sigint_handler()
{
  kill $(jobs -p)
  exit
}

trap sigint_handler SIGINT

PORT=7249
TARGET_DIR=bin
TARGET=start-servant
CMD=$1
BUILD_DIR=dist-$TARGET-$CMD
ADDITIONAL_WATCH=""

while true; do
  # $@ &
    case $CMD in
	haddock) cabal haddock &
		 ;;
	ghcid) ghcid -l -c "cabal new-repl $TARGET --builddir $BUILD_DIR" &
	       ;;
	run) cabal run $TARGET \
		   --disable-optimisation \
		   --builddir $BUILD_DIR -- \
		   stm \
		   -M New \
	     &
	     ADDITIONAL_WATCH="-r src"
	     ;;
	*) echo "Unknown cmd ('$CMD'), must be either 'run','ghcid' or 'haddock'"
	   break;
	   ;;
    esac

  # run_server &
  PID1=$!

  inotifywait \
      -e modify \
      -e move \
      -e create \
      -e delete \
      -e attrib \
      $ADDITIONAL_WATCH \
      -r $TARGET_DIR \
      $TARGET.cabal \
      --exclude ".*flycheck.*|.*\#.*"
  kill $(jobs -p)
  fuser -k $PORT/tcp
  sleep 3
done
