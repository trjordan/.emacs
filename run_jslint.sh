#!/bin/sh

path=`dirname $0`

echo -n /*jslint white: false, forin: true, maxlen: 120, regexp: false, newcap: true*/ /*global Ext: false, TST: false*/ | cat - $1 > $path/tmp.js
rhino $path/jslint.js $path/tmp.js
rm $path/tmp.js