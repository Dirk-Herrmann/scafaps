#!/bin/bash

# Copyright (C) 2023-2026 Dirk Herrmann

set -o errexit -o pipefail -o noclobber -o nounset

scafaps=../src/scafaps.py
regexpify=../src/regexpify.sh

function validate () {
   if diff -q "${expected}" "${output}"; then
      echo "PASSED"
   else
      echo "FAILED: ${toolname} test '${testname}'"
   fi
}

find . -maxdepth 1 -type f -name 'test_*' -printf '%f\0' \
| sed -z 's/^test_//;s/_/ /;s/_.*//' | sort -k1r,1 -k2,2 -z | uniq -z \
| while IFS=' ' read -d '' -r toolname testname; do
   echo "About to execute ${toolname} test '${testname}'"

   suppressions="test_${toolname}_${testname}_suppressions.txt"
   test -e "${suppressions}" || suppressions="default_suppressions.txt"
   lines="test_${toolname}_${testname}_lines.txt"
   test -e "${lines}" || lines="default_lines.txt"
   output="test_${toolname}_${testname}_output.txt"
   rm -f "${output}"
   expected="test_${toolname}_${testname}_expected.txt"

   script="test_${toolname}_${testname}_script.sh"
   if test -f "${script}"; then
      source "${script}"
   elif [[ ${toolname} == "scafaps" ]]; then
      "${scafaps}" "${suppressions}" < "${lines}" >& "${output}" || true
      validate
   elif [[ ${toolname} == "regexpify" ]]; then
      "${regexpify}" < "${lines}" >& "${output}"
      validate
   fi
done
