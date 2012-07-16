#!/bin/bash

VALIDATORS="tools/cpp-validator/lambdamine tools/hs-validator/validator tools/python-validator/validate.py"

for v in $VALIDATORS; do
  echo "Testing $v..."
  unittests/runtests.sh $v
done
