#!/bin/echo file for sourcing only

# Copyright (C) 2023 Dirk Herrmann

# Have to re-define suppressions to non-existing file
suppressions="test_${toolname}_${testname}_suppressions.txt"

"${scafaps}" "${suppressions}" < "${lines}" >& "${output}" || true
validate
