#!/bin/bash

SOLL_DIR="./soll_output"
IST_DIR="./output"

./generate-local-tests.sh

uvx --python 3.10 pytest ./generated-python-output/debug

for i in 010 011 012 121 122 123; do 
  echo "=================================== Comparing Test $i ==================================="
  echo "SOLL (C):"
  xxd -b "$SOLL_DIR/test_case_ACN_000$i.dat"
  echo ""
  echo "IST (PY):"
  xxd -b "$IST_DIR/test_case_ACN_000$i.dat"
  echo ""
  echo "Comparison:"
  cmp "$SOLL_DIR/test_case_ACN_000$i.dat" "$IST_DIR/test_case_ACN_000$i.dat"
  echo ""
done