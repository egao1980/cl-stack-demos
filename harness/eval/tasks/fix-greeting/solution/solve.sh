#!/bin/bash
# Reference solution: fix the misspelt variable.
sed -i.bak 's/\$nme/$name/' greet.sh && rm -f greet.sh.bak
