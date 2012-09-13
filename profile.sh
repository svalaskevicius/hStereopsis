#!/bin/sh

cabal configure --enable-executable-profiling --ghc-option=-auto-all
cabal build
time ./dist/build/hStereopsis/hStereopsis data/TeaPotStereoData/image0001_left.png +RTS -N4 -p -K100M -hc -sresult.summary
cat hStereopsis.hp | hp2ps -c -s | pstopdf -i -o hStereopsis.pdf
