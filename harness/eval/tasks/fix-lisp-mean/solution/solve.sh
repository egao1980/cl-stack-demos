#!/bin/bash
# Reference solution: divide by the length, not the length plus one.
sed -i.bak 's/(1+ (length numbers))/(length numbers)/' stats.lisp && rm -f stats.lisp.bak
