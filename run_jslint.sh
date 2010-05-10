#!/bin/sh

path=`dirname $0`

echo -n /*jslint newcap: false, undef: false*/ | cat - $1 | $path/jslint.js