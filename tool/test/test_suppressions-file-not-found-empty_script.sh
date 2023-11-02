#!/bin/echo file for sourcing only

# Copyright (C) 2023 Dirk Herrmann

# Have to re-define suppressions to non-existing file
suppressions="test_${t}_suppressions.txt"

"${scafaps}" --suppressions-file-not-found=empty "${suppressions}" < "${lines}" >& "${output}"
validate
