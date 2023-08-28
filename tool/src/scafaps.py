#!/usr/bin/env python3

# Copyright (C) 2023 Dirk Herrmann

import argparse
import dataclasses
import os
import re
import sys

# Verbosity levels:
# 0: normal program output
# 1: support user (explain results)
# 2: support user (debugging user input)
# 3: dev debug (show internal results)
# 4: dev debug (show log levels)
verbosity = 0
def log(level, message):
   global verbosity
   if verbosity > 3:
      print(f'LOG({level}): {message}')
   elif level <= verbosity:
      print(message)

def read_raw_lines(input_file):
   # read whole file, drop \n from lines
   return input_file.read().splitlines()

@dataclasses.dataclass
class Line:
   linenr: int
   content: str

def read_lines(input_file):
   raw_lines = read_raw_lines(input_file)
   lines = [Line(nr, raw_lines[nr]) for nr in range(len(raw_lines))]
   return lines

@dataclasses.dataclass
class Regexp:
   linenr: int
   content: str
   regexp: re.Pattern

# TODO: Allow convenience patterns in suppressions, like %path%
def read_suppression_file(path):
   with open(path) as input_file:
      raw_lines = read_raw_lines(input_file)
      regexps = []
      errors = 0
      for nr, raw_line in enumerate(raw_lines):
         if len(raw_line) > 0 and raw_line[0] == "#":
            # ignore comment lines and shebang line
            continue
         try:
            regexp = re.compile(raw_line)
            regexps.append(Regexp(nr, raw_line, regexp))
         except re.error as e:
            errors += 1
            print(f'Error compiling suppression in line {nr}: {e.msg}')
            print(f'  Offending suppression: >>{raw_line}<<')
      if errors > 0:
         raise ValueError(f'Compilation errors in {errors} suppressions')
      else:
         return regexps

# Function showMatch is factored out, because it is used at two different
# places: when computing the initial matching sequence, and when showing the
# results after computing the lcsTable
def showMatch(regexps, lines, i, j):
   # show only in verbose mode
   log(1, 'Match:')
   log(1, f'~ {regexps[i].linenr}: {regexps[i].content}')
   log(1, f'= {lines[j].linenr}: {lines[j].content}')

# Find and show matches between regexps[i] and lines[i] for i=0..
def computeInitialMatchingSequence(regexps, lines):
   for index, (regexp, line) in enumerate(zip(regexps, lines)):
      if regexp.regexp.fullmatch(line.content):
         showMatch(regexps, lines, index, index)
      else:
         # index of the first mismatch is the length so far
         return index
   # length is shorter of the two lists (which may be empty)
   return min(len(regexps), len(lines))

# TODO: Optimize time by only computing matches for fields that can actually
# become part of the lcs
def computeLcsTable(regexps, lines):
   array = [[0] * (len(lines) + 1) for r in range(len(regexps) + 1)]
   for i in range(1, len(regexps) + 1):
      for j in range(1, len(lines) + 1):
         if regexps[i-1].regexp.fullmatch(lines[j-1].content):
            array[i][j] = array[i-1][j-1] + 1
         else:
            array[i][j] = max(array[i][j-1], array[i-1][j])
   return array

# TODO: Convert to iterative solution
def recursiveShowDiffs(lcsTable, regexps, lines, i, j):
   if i == 0 and j == 0:
      return (0, 0)
   elif i == 0:
      counts = recursiveShowDiffs(lcsTable, regexps, lines, i, j-1)
      log(3, 'Unmatched input line at head of input lines')
      log(1, 'Unmatched input line:')
      print(f'+ {lines[j-1].linenr}: {lines[j-1].content}')
      return (counts[0], counts[1] + 1)
   elif j == 0:
      counts = recursiveShowDiffs(lcsTable, regexps, lines, i-1, j)
      log(3, 'Unmatched suppression at head of suppressions')
      log(1, 'Unmatched suppression:')
      print(f'- {regexps[i-1].linenr}: {regexps[i-1].content}')
      return (counts[0] + 1, counts[1])
   elif regexps[i-1].regexp.fullmatch(lines[j-1].content):
      # Line matches - if several possibilities exist, prefer matches against
      # earlier lines, i.e. show diffs against later lines.
      if lcsTable[i][j-1] == lcsTable[i][j]:
         # Some previous line would fit as well
         counts = recursiveShowDiffs(lcsTable, regexps, lines, i, j-1)
         log(3, 'Deliberately preferring unmatched line over match against previous suppression')
         log(1, 'Unmatched input line:')
         print(f'+ {lines[j-1].linenr}: {lines[j-1].content}')
         return (counts[0], counts[1] + 1)
      else:
         # Take this match
         counts = recursiveShowDiffs(lcsTable, regexps, lines, i-1, j-1)
         showMatch(regexps, lines, i-1, j-1)
         return counts
   elif lcsTable[i][j-1] > lcsTable[i-1][j]:
      counts = recursiveShowDiffs(lcsTable, regexps, lines, i, j-1)
      log(3, 'Unmatched input line necessary to achieve lcs')
      log(1, 'Unmatched input line:')
      print(f'+ {lines[j-1].linenr}: {lines[j-1].content}')
      return (counts[0], counts[1] + 1)
   elif lcsTable[i][j-1] == lcsTable[i][j]:
      counts = recursiveShowDiffs(lcsTable, regexps, lines, i-1, j)
      log(3, 'Show unmatched suppressions after unmatched lines')
      log(1, 'Unmatched suppression:')
      print(f'- {regexps[i-1].linenr}: {regexps[i-1].content}')
      return (counts[0] + 1, counts[1])
   else:
      assert lcsTable[i][j-1] != lcsTable[i-1][j] # not possible / covered
      counts = recursiveShowDiffs(lcsTable, regexps, lines, i-1, j)
      log(3, 'Unmatched suppression necessary to achieve lcs')
      log(1, 'Unmatched suppression:')
      print(f'- {regexps[i-1].linenr}: {regexps[i-1].content}')
      return (counts[0] + 1, counts[1])

def showDiffsFromLcsTable(lcsTable, regexps, lines):
   # Python's default recursion limit is only 1000
   sys.setrecursionlimit(max(1000, len(regexps) + len(lines)))
   counts = recursiveShowDiffs(lcsTable, regexps, lines, len(regexps), len(lines))
   return counts

def parse_arguments():
   parser = argparse.ArgumentParser(
      description='Suppress false positives from static code analysis.' )
   parser.add_argument(
      'suppressions', metavar='suppressions-file', type=file_name,
      help=
         'file with suppressions to be applied' )
   parser.add_argument(
      '--error', '-e', action='store_true',
      help=
         'exit with error if unsuppressed output remains' )
   parser.add_argument(
      '--error-unused', '-u', action='store_true',
      help=
         'exit with error if there are unused suppressions' )
   parser.add_argument(
      '--verbose', '-v', action='count', default=0,
      help=
         'increase verbosity level.  The option can be given several times' )
   parser.add_argument(
      '--version', action='version', version='%(prog)s 0.1.0-pre' )

   args = parser.parse_args()

   global verbosity
   verbosity = args.verbose

   return args

def file_name(string : str):
   if os.path.isfile(string):
      return string
   else:
      raise argparse.ArgumentTypeError(f'\'{string}\' does not name a file')

def run_scafaps():
   args = parse_arguments()
   log(2, f'Option settings: {vars(args)}')

   try:
      log(1, f'Reading suppressions from \'{args.suppressions}\'')
      regexps = read_suppression_file(args.suppressions)
      log(2, f'Suppression regexps: {regexps}')
   except ValueError as v:
      print(str(v))
      sys.exit(1)

   log(1, 'Reading input lines (SCA output) from stdin')
   lines = read_lines(sys.stdin) # TODO: allow named file to be given on command line
   log(2, f'Input lines: {lines}')

   skip = computeInitialMatchingSequence(regexps, lines)
   log(3, f'initial matching sequence length: {skip}')
   regexps = regexps[skip:]
   lines   = lines[skip:]

   lcsTable = computeLcsTable(regexps, lines)
   log(3, f'lcsTable: {lcsTable}')

   counts = showDiffsFromLcsTable(lcsTable, regexps, lines)
   print(f'Unmatched input lines: {counts[1]}')
   print(f'Unmatched suppressions: {counts[0]}')

   if counts[1] > 0 and args.error:
      log(1, 'There were unmatched input lines, exiting with error.')
      sys.exit(1)
   elif counts[0] > 0 and args.error_unused:
      log(1, 'There were unmatched suppressions, exiting with error.')
      sys.exit(1)
   else:
      log(1, 'Exiting successfully.')
      sys.exit(0)

if __name__ == "__main__":
   run_scafaps()
