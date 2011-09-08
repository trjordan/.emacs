#!/bin/sh
#
# Runs JSLint on a given file, emitting warnings from the tracelytics-specific
# options file.

path=`dirname $0`

cat $path/jslint-comment-header | tr '\n' ' ' > $path/jslint-comment-header-oneline
cat $path/jslint-comment-header-oneline $1 > $path/tmp.js
rhino $path/jslint.js $path/tmp.js
rm $path/tmp.js