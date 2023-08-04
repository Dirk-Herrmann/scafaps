#!/bin/bash

# Copyright (C) 2023 Dirk Herrmann

scafaps=../src/scafaps.py

ls -1 | grep '^test_' | sed 's/^test_//;s/_.*//' | uniq | while read -r t; do
   echo "About to execute test '${t}'"
   script="test_${t}_script.sh"
   if test -f "$script"; then
      bash "$script"
   else
      suppressions="test_${t}_suppressions.txt"
      test -e "${suppressions}" || suppressions="default_suppressions.txt"
      lines="test_${t}_lines.txt"
      test -e "${lines}" || lines="default_lines.txt"
      output="test_${t}_output.txt"
      $scafaps "${suppressions}" < "${lines}" >& "${output}"
      expected="test_${t}_expected.txt"
      if diff -q "${expected}" "${output}"; then
         echo "PASSED"
      else
         echo "FAILED: ${t}"
      fi
   fi
done
