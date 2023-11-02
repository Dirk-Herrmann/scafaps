#!/bin/bash

# Copyright (C) 2023 Dirk Herrmann

set -o errexit -o pipefail -o noclobber -o nounset

scafaps=../src/scafaps.py
regexpify=../src/regexpify.sh

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
   rm -f "${output}"
   expected="test_${t}_expected.txt"

   script="test_${t}_script.sh"
   if test -f "${script}"; then
      source "${script}"
   else
      ${scafaps} "${suppressions}" < "${lines}" >& "${output}" || true
      validate
   fi
done
