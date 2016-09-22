#!/bin/bash

# Generating arithmetic expression
./generate > test

echo "print(" $(cat test) ")" > pythontest
python3 pythontest

rm test
rm pythontest
