#!/bin/sh

grep "^[A-Za-z]* (" $1 | sed -e 's/ (.*//'
