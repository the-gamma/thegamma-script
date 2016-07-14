#!/bin/bash
(cd src/thegamma; node ../../paket-files/github.com/fsprojects/Fable/build/fable/ -w) &
(cd src/libraries; node ../../paket-files/github.com/fsprojects/Fable/build/fable/ -w) &
(cd src; node ../paket-files/github.com/fsprojects/Fable/build/fable/ -w) &
wait
