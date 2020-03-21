#!/bin/bash

set -e

bash build.sh
(pushd www; python -m SimpleHTTPServer; popd) & 

dune build server/server.exe
dune exec server/server.exe &

trap 'kill $(jobs -p)' EXIT

sleep infinity
