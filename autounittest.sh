#!/bin/sh
while :; do
    inotifywait -e modify --exclude '.*.swp' src test
    ./rebar skip_deps=true eunit
done
