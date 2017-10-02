#!/bin/bash

stack clean
# ghc-options: -O2 -threaded -fprof-auto "-with-rtsopts=-N -p -s -h -i0.1"
#stack configure --user --enable-executable-profiling --ghc-options="-rtsopts -prof -fprof-auto"
stack build --profile

rm kage-exe.aux
rm kage-exe.hp
rm kage-exe.prof
rm kage-exe.ps
rm kage-exe.summary

stack exec kage-exe -- +RTS -p -hc -skage-exe.summary -K1G

stack exec hp2ps -- kage-exe.hp prof/kage-exe.ps
cat kage-exe.summary

