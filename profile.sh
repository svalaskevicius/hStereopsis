#!/bin/sh

cabal-dev configure --enable-executable-profiling --ghc-option=-auto-all
cabal-dev build
time ./dist/build/hStereopsis/hStereopsis data/TeaPotStereoData/image0001_left.png data/TeaPotStereoData/image0001_right.png +RTS -N4 -p -K100M -hc -sresult.summary
cat hStereopsis.hp | hp2ps -c -s | ps2pdf - hStereopsis.pdf
