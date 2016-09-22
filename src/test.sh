#!/bin/bash

# Generating arithmetic expression
./generate > test

echo "print(" $(cat test) ")" > pythontest

echo "Python3 evaluates to :"
python3 pythontest

echo "Our parser evaluates to :"
./expr

echo ""

rm test
rm pythontest
