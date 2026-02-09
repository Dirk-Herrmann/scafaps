#!/usr/bin/env python3

# Copyright (C) 2023 Dirk Herrmann

import argparse
import dataclasses
import pathlib
import re
import sys

# Verbosity levels:
# 0: normal program output
# 1: support user (io, also show matches)
# 2: support user (explain matching results)
# 3: support user (debugging user input)
# 4: dev debug (show internal results)
# 5: dev debug (show log levels)
verbosity = 0
def getLogString(level, message):
   global verbosity
   if verbosity > 4:
      return f'LOG({level}): {message}'
   elif level <= verbosity:
      return message
   else:
      return False

def log(level, message):
   logString = getLogString(level, message)
   if logString:
      print(logString)

def logAppend(logs, level, message):
   logString = getLogString(level, message)
   if logString:
      logs.append(logString)

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
def read_suppressions_file(path):
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
         except re.error as e:
            errors += 1
            print(f'Error compiling suppression in line {nr}: {e.msg}')
            print(f'  Offending suppression: >>{raw_line}<<')
         else:
            regexps.append(Regexp(nr, raw_line, regexp))
      if errors > 0:
         raise ValueError(f'Compilation errors in {errors} suppressions')
      else:
         return regexps

# Function showMatch is factored out, because it is used at two different
# places: when computing the initial matching sequence, and when showing the
# results after computing the lcsTable
def showMatch(regexps, lines, i, j):
   # show only in verbose mode
   log(2, 'Match:')
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

# The resulting table contains the maximum number of matching lines for the
# respective lists of lines.  That is, array[i][j] gives the maximum number of
# matching lines found between the first i lines from the regex list and the
# first j lines from the text lines.
def computeLcsTable(regexps, lines):
   array = [[0] * (len(lines) + 1) for r in range(len(regexps) + 1)]
   for i in range(1, len(regexps) + 1):
      for j in range(1, len(lines) + 1):
         if regexps[i-1].regexp.fullmatch(lines[j-1].content):
            array[i][j] = array[i-1][j-1] + 1
         else:
            array[i][j] = max(array[i][j-1], array[i-1][j])
   return array

def showDiffsFromLcsTable(lcsTable, regexps, lines):
   reversed_output = [] # will be reversed before printing
   unmatched_regexps = 0
   unmatched_lines   = 0
   i = len(regexps)
   j = len(lines)
   while i > 0 or j > 0:
      tmp_output = []
      if i == 0:
         logAppend(tmp_output, 4, 'Unmatched input line at head of input lines')
         logAppend(tmp_output, 2, 'Unmatched input line:')
         logAppend(tmp_output, 0, f'+ {lines[j-1].linenr}: {lines[j-1].content}')
         unmatched_lines += 1
         j = j - 1
      elif j == 0:
         logAppend(tmp_output, 4, 'Unmatched suppression at head of suppressions')
         logAppend(tmp_output, 2, 'Unmatched suppression:')
         logAppend(tmp_output, 0, f'- {regexps[i-1].linenr}: {regexps[i-1].content}')
         unmatched_regexps += 1
         i = i -1
      elif regexps[i-1].regexp.fullmatch(lines[j-1].content):
         # Line matches - if several possibilities exist, prefer matches against
         # earlier lines, i.e. show diffs against later lines.
         if lcsTable[i][j-1] == lcsTable[i][j]:
            # Some previous line would fit as well
            logAppend(tmp_output, 4, 'Deliberately preferring unmatched line over match against previous suppression')
            logAppend(tmp_output, 2, 'Unmatched input line:')
            logAppend(tmp_output, 0, f'+ {lines[j-1].linenr}: {lines[j-1].content}')
            unmatched_lines += 1
            j = j - 1
         else:
            # Take this match
            logAppend(tmp_output, 2, 'Match:')
            logAppend(tmp_output, 1, f'~ {regexps[i-1].linenr}: {regexps[i-1].content}')
            logAppend(tmp_output, 1, f'= {lines[j-1].linenr}: {lines[j-1].content}')
            i = i - 1
            j = j - 1
      elif lcsTable[i][j-1] > lcsTable[i-1][j]:
         logAppend(tmp_output, 4, 'Unmatched input line necessary to achieve lcs')
         logAppend(tmp_output, 2, 'Unmatched input line:')
         logAppend(tmp_output, 0, f'+ {lines[j-1].linenr}: {lines[j-1].content}')
         unmatched_lines += 1
         j = j - 1
      elif lcsTable[i][j-1] == lcsTable[i][j]:
         logAppend(tmp_output, 4, 'Show unmatched suppressions after unmatched lines')
         logAppend(tmp_output, 2, 'Unmatched suppression:')
         logAppend(tmp_output, 0, f'- {regexps[i-1].linenr}: {regexps[i-1].content}')
         unmatched_regexps += 1
         i = i - 1
      else:
         assert lcsTable[i][j-1] != lcsTable[i-1][j] # not possible / covered
         logAppend(tmp_output, 4, 'Unmatched suppression necessary to achieve lcs')
         logAppend(tmp_output, 2, 'Unmatched suppression:')
         logAppend(tmp_output, 0, f'- {regexps[i-1].linenr}: {regexps[i-1].content}')
         unmatched_regexps += 1
         i = i - 1
      reversed_output.extend(reversed(tmp_output))
   for message in reversed(reversed_output):
      print(message)
   return (unmatched_regexps, unmatched_lines)

def logDiffsSummary(level, counts):
   log(level, f'Unmatched input lines: {counts[1]}')
   log(level, f'Unmatched suppressions: {counts[0]}')

def copyInputToOutput():
   while True:
      data = sys.stdin.read()
      sys.stdout.write(data)
      if len(data) == 0:
         return

def parse_arguments():
   parser = argparse.ArgumentParser(
      description='Suppress false positives from static code analysis.' )
   parser.add_argument(
      'suppressions', metavar='suppressions-file', type=pathlib.Path,
      help=
         'file with suppressions to be applied' )
   parser.add_argument(
      '--suppressions-file-not-found',
      choices=['error', 'empty', 'pass'], default='error',
      help=
         'how to proceed if the suppressions-file does not exist.  If "error" '
         'is selected (the default), scafaps will exit with an error code.  '
         'With "empty", scafaps will behave as if an empty suppressions-file '
         'was given.  With "pass", stdin will be copied directly to stdout as '
         'if scafaps was not there.')
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

def run_scafaps():
   args = parse_arguments()
   log(3, f'Option settings: {vars(args)}')

   if args.suppressions.is_file():
      log(1, f'Reading suppressions from \'{args.suppressions}\'')
      try:
         regexps = read_suppressions_file(args.suppressions)
      except ValueError as v:
         print(str(v))
         sys.exit(1)
      log(3, f'Suppression regexps: {regexps}')
   else:
      notfoundmsg = f'Suppressions-file \'{args.suppressions}\' not found'
      if args.suppressions_file_not_found == 'error':
         print(f'Error: {notfoundmsg}')
         sys.exit(1)
      elif args.suppressions_file_not_found == 'empty':
         log(1, f'{notfoundmsg}, treating it as an empty file')
         regexps = []
      elif args.suppressions_file_not_found == 'pass':
         log(1, f'{notfoundmsg}, passing input data through')
         copyInputToOutput()
         sys.exit(0)

   log(1, 'Reading input lines (SCA output) from stdin')
   lines = read_lines(sys.stdin) # TODO: allow named file to be given on command line
   log(3, f'Input lines: {lines}')

   skip = computeInitialMatchingSequence(regexps, lines)
   log(4, f'initial matching sequence length: {skip}')
   regexps = regexps[skip:]
   lines   = lines[skip:]

   lcsTable = computeLcsTable(regexps, lines)
   log(4, f'lcsTable: {lcsTable}')

   counts = showDiffsFromLcsTable(lcsTable, regexps, lines)

   if counts[1] > 0:
      logDiffsSummary(0, counts)
      if args.error:
         log(1, 'There were unmatched input lines, exiting with error.')
         sys.exit(1)
   elif counts[0] > 0:
      logDiffsSummary(0, counts)
      if args.error_unused:
         log(1, 'There were unmatched suppressions, exiting with error.')
         sys.exit(1)
   else:
      logDiffsSummary(1, counts) # only show in verbose mode
      log(1, 'Exiting successfully.')
      sys.exit(0)

if __name__ == "__main__":
   run_scafaps()
