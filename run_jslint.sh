#!/bin/sh

path=`dirname $0`

cat $path/jslint-comment-header $1 > $path/tmp.js
rhino $path/jslint.js $path/tmp.js
#rm $path/tmp.js