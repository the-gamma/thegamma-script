#!/bin/bash
(cd src/thegamma; node ../../node_modules/fable-compiler/fable/ -w) &
(cd src/libraries; node ../../node_modules/fable-compiler/fable/ -w) &
(cd src/gui; node ../../node_modules/fable-compiler/fable/ -w) &
(cd src/main; node ../../node_modules/fable-compiler/fable/ -w) &
(node node_modules/webpack/bin/webpack --watch)
wait
