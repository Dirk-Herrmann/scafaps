#!/bin/bash

# Copyright (C) 2023 Dirk Herrmann

set -o noclobber -o nounset

rootdir=../../
scafaps="${rootdir}/tool/src/scafaps.py"

##############################################################################
# Shell scripts are checked with shellcheck
##############################################################################
find "${rootdir}" \
   \( -name .git -o -path '*/experiments/bash' \) -prune \
   -o -type f -exec sh -c 'file "$1" | grep -q "shell script"' shell {} \; -print0 \
| while IFS='' read -d '' -r scriptfullname; do
   echo "Running shellcheck on ${scriptfullname}..."
   scriptdir="$(dirname "${scriptfullname}")"
   scriptname="$(basename "${scriptfullname}")"
   scafapsfile="${scriptdir}/.scafaps/${scriptname}.shellcheck.suppress"
   shellcheck -o all "${scriptfullname}" \
      | # add --verbose option to see also the matching line pairs:
        ${scafaps} --verbose --suppressions-file-not-found=empty "${scafapsfile}" \
      | # of the matching line pairs, keep the input lines, drop the regexps
        grep -v '^[~]' \
      | # filter the mismatches with some context lines before and after
        grep -B 5 -A 2 '^[-+]' \
      | # color unmatched regexps in red.  --color=always needed as grep does
        # otherwise not add color when writing to a pipe.  We keep also the
        # non-regexp lines by alternative match against '$'.
        GREP_COLORS='ms=01;31' grep -E --color=always '^[-].*|$' \
      | # similarly with green for unmatched lines
        GREP_COLORS='ms=01;32' grep -E --color=always '^[+].*|$'
done
