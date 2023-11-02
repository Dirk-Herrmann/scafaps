#!/bin/echo file for sourcing only

# Copyright (C) 2023 Dirk Herrmann

"${regexpify}" < "${lines}" >& "${output}"
validate
