#!/bin/bash

# Copyright (C) 2023 Dirk Herrmann

# Have to re-define suppressions to non-existing file
suppressions="test_${t}_suppressions.txt"

$scafaps --suppressions-file-not-found=error "${suppressions}" < "${lines}" >& "${output}"
validate