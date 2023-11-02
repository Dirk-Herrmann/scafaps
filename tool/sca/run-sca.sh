#!/bin/bash

# Copyright (C) 2023 Dirk Herrmann

set -o errexit -o pipefail -o noclobber -o nounset

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
   { shellcheck -o all "${scriptfullname}" || true; } \
      | ${scafaps} --suppressions-file-not-found=pass "${scafapsfile}"
done
