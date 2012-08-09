#!/bin/sh
while :; do
    inotifywait -e modify --exclude '.*.swp' src test
    ./rebar -v skip_deps=true eunit
done
