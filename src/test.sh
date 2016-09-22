#!/bin/bash

# Generating arithmetic expression
./generate > test

cat test
echo 

echo "print(" $(cat test) ")" > pythontest

echo "Python3 evaluates to :"
python3 pythontest

echo "Our parser evaluates to :"
./expr_parser

echo ""

rm test
rm pythontest
