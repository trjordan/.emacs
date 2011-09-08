#!/bin/sh

path=`dirname $0`

~/tlenv/bin/pylint --rcfile=~/.pylintrc $1

