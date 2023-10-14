#!/bin/bash

# Copyright (C) 2023 Dirk Herrmann

scafaps=../src/scafaps.py

function validate () {
   if diff -q "${expected}" "${output}"; then
      echo "PASSED"
   else
      echo "FAILED: ${t}"
   fi
}

LC_ALL=C ls -1 | grep '^test_' | sed 's/^test_//;s/_.*//' | uniq | while read -r t; do
   echo "About to execute test '${t}'"

   suppressions="test_${t}_suppressions.txt"
   test -e "${suppressions}" || suppressions="default_suppressions.txt"
   lines="test_${t}_lines.txt"
   test -e "${lines}" || lines="default_lines.txt"
   output="test_${t}_output.txt"
   expected="test_${t}_expected.txt"

   script="test_${t}_script.sh"
   if test -f "$script"; then
      source "$script"
   else
      $scafaps "${suppressions}" < "${lines}" >& "${output}"
      validate
   fi
done
