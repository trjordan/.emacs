#!/bin/sh

path=`dirname $0`

cat $path/jslint-comment-header | tr '\n' ' ' > jslint-comment-header-oneline
cat jslint-comment-header-oneline $1 > $path/tmp.js
rhino $path/jslint.js $path/tmp.js
rm $path/tmp.js