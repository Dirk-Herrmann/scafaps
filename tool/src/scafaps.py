#!/usr/bin/env python3

# Copyright (C) 2023-2026 Dirk Herrmann

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
# 5: dev debug (show verbosity levels)
verbosity = 0 # will be set by parse_arguments
def maybe_get_output(level, message):
   global verbosity
   if verbosity >= 5:
      return [f'Verbosity({level}): {message}']
   elif level <= verbosity:
      return [message]
   else:
      return []

def output(level, message):
   if (output_list := maybe_get_output(level, message)):
      print(output_list[0])

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
   valid: bool
   regexp: re.Pattern
   comments: list

# TODO: Allow convenience patterns in suppressions, like %path%
def read_suppressions_file(path):
   with open(path) as input_file:
      raw_lines = read_raw_lines(input_file)
      regexps = []
      comments = []
      errors = 0
      never_matches = re.compile("$a")
      for nr, raw_line in enumerate(raw_lines):
         if len(raw_line) > 0 and raw_line[0] == "#":
            comments.append(Line(nr, raw_line))
            continue
         try:
            regexp = re.compile(raw_line)
         except re.error as e:
            errors += 1
            print(f'Error compiling suppression in line {nr}: {e.msg}')
            print(f'  Offending suppression: >>{raw_line}<<')
            regexps.append(Regexp(
               nr, raw_line, False, never_matches, comments))
         else:
            regexps.append(Regexp(
               nr, raw_line, True, regexp, comments))
            comments = []
      return regexps, comments, errors

max_linenr_width = 0 # will be set by run_scafaps
def get_numbered_output_string(prefix, line, valid=True):
   error_indicator = ' ' if valid else '*'
   return f'{prefix}{error_indicator}{line.linenr:{max_linenr_width}}: {line.content}'

def get_comments_output(comments):
   output = []
   for comment in comments:
      output += maybe_get_output(1, get_numbered_output_string('#', comment))
   return output

def get_match_output(regexps, lines, i, j):
   output =  get_comments_output(regexps[i].comments)
   output += maybe_get_output(2, 'Match:')
   output += maybe_get_output(1, get_numbered_output_string('~', regexps[i]))
   output += maybe_get_output(1, get_numbered_output_string('=', lines[j]))
   return output

def get_unmatched_suppression_output(debug_msgs, regexp):
   # prepend comment lines from suppression file to possible debug messages
   output =  get_comments_output(regexp.comments) + debug_msgs
   output += maybe_get_output(2, 'Unmatched suppression:')
   valid  = regexp.valid # indicate whether the suppression could be compiled
   output += maybe_get_output(0, get_numbered_output_string('-', regexp, valid=valid))
   return output

# Find and show matches between regexps[i] and lines[i] for i=0..
def compute_initial_matching_sequence(regexps, lines):
   for index, (regexp, line) in enumerate(zip(regexps, lines)):
      if regexp.regexp.fullmatch(line.content):
         for message in get_match_output(regexps, lines, index, index):
            print(message)
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
def compute_lcs_table(regexps, lines):
   array = [[0] * (len(lines) + 1) for r in range(len(regexps) + 1)]
   for i in range(1, len(regexps) + 1):
      for j in range(1, len(lines) + 1):
         if regexps[i-1].regexp.fullmatch(lines[j-1].content):
            array[i][j] = array[i-1][j-1] + 1
         else:
            array[i][j] = max(array[i][j-1], array[i-1][j])
   return array

# Iterate through the lcs table and show the diff results.  The table is
# traversed from the end of the table to its origin.  The output, in contrast,
# shall be printed from the first lines to the last.  For this reason, the
# output is created in a list and only finally printed in the proper order.
def show_diffs_from_lcs_table(lcs_table, regexps, lines):
   reversed_output = [] # will be reversed before printing

   unmatched_regexps = 0
   unmatched_lines = 0

   i = len(regexps)
   j = len(lines)
   while i > 0 or j > 0:
      # within the loop, the output collected in a small list (tmp_output) and
      # at the end of each iteration added to the function's overall output.
      tmp_output = []

      # constant identifiers for possible results, used only within loop body
      UNMATCHED_INPUT_LINE  = 0
      UNMATCHED_SUPPRESSION = 1
      MATCH                 = 2

      # discriminate the different scenarios for lcs table index i,j
      if i == 0:
         tmp_output = maybe_get_output(4, 'Unmatched input line at head of input lines')
         result = UNMATCHED_INPUT_LINE
      elif j == 0:
         tmp_output = maybe_get_output(4, 'Unmatched suppression at head of suppressions')
         result = UNMATCHED_SUPPRESSION
      elif regexps[i-1].regexp.fullmatch(lines[j-1].content):
         # Line matches - if several possibilities exist, prefer matches against
         # earlier lines, i.e. show diffs against later lines.
         if lcs_table[i][j-1] == lcs_table[i][j]:
            # Some previous line would fit as well
            tmp_output = maybe_get_output(4, 'Deliberately preferring unmatched line')
            result = UNMATCHED_INPUT_LINE
         elif lcs_table[i-1][j] == lcs_table[i][j]:
            # Some previous suppression would fit as well
            tmp_output = maybe_get_output(4, 'Deliberately preferring unmatched suppression')
            result = UNMATCHED_SUPPRESSION
         else:
            result = MATCH
      elif lcs_table[i][j-1] > lcs_table[i-1][j]:
         tmp_output = maybe_get_output(4, 'Unmatched input line necessary to achieve lcs')
         result = UNMATCHED_INPUT_LINE
      elif lcs_table[i][j-1] == lcs_table[i-1][j]:
         tmp_output = maybe_get_output(4, 'Show unmatched suppressions before unmatched lines')
         result = UNMATCHED_INPUT_LINE
      else:
         assert lcs_table[i][j-1] < lcs_table[i-1][j]
         tmp_output = maybe_get_output(4, 'Unmatched suppression necessary to achieve lcs')
         result = UNMATCHED_SUPPRESSION

      # extend output and update variables according to result
      if result == UNMATCHED_SUPPRESSION:
         tmp_output = get_unmatched_suppression_output(tmp_output, regexps[i-1])
         unmatched_regexps += 1
         i -= 1
      elif result == UNMATCHED_INPUT_LINE:
         tmp_output += maybe_get_output(2, 'Unmatched input line:')
         tmp_output += maybe_get_output(0, get_numbered_output_string('+', lines[j-1]))
         unmatched_lines += 1
         j -= 1
      else: # result == MATCH
         tmp_output += get_match_output(regexps, lines, i-1, j-1)
         i -= 1
         j -= 1

      # add the output from the loop body to the function's overall output
      reversed_output += reversed(tmp_output)

   for message in reversed(reversed_output):
      print(message)
   return (unmatched_regexps, unmatched_lines)

def output_diffs_summary(level, counts):
   output(level, f'Unmatched input lines: {counts[1]}')
   output(level, f'Unmatched suppressions: {counts[0]}')

def copy_input_to_output(input_file):
   while True:
      data = input_file.read()
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
      '--keep-going-with-compile-errors', action='store_true',
      help=
         'keep going even if there are errors when compiling suppressions' )
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
   output(3, f'Option settings: {vars(args)}')

   if args.suppressions.is_file():
      output(1, f'Reading suppressions from \'{args.suppressions}\'')
      regexps, tail_comments, errors = read_suppressions_file(args.suppressions)
      if errors:
         print(f'Compilation errors in {errors} suppressions')
         if not args.keep_going_with_compile_errors:
            sys.exit(1)
      output(3, f'Suppression regexps: {regexps}')
   else:
      notfoundmsg = f'Suppressions-file \'{args.suppressions}\' not found'
      if args.suppressions_file_not_found == 'error':
         print(f'Error: {notfoundmsg}')
         sys.exit(1)
      elif args.suppressions_file_not_found == 'empty':
         output(1, f'{notfoundmsg}, treating it as an empty file')
         regexps, tail_comments = [], []
      elif args.suppressions_file_not_found == 'pass':
         output(1, f'{notfoundmsg}, passing input data through')
         copy_input_to_output(sys.stdin)
         sys.exit(0)

   output(1, 'Reading input lines (SCA output) from stdin')
   lines = read_lines(sys.stdin) # TODO: allow named file to be given on command line
   output(3, f'Input lines: {lines}')

   global max_linenr_width
   max_lines_linenr = lines[-1].linenr if lines else 0
   max_regexps_linenr = regexps[-1].linenr if regexps else 0
   max_comments_linenr = tail_comments[-1].linenr if tail_comments else 0
   max_linenr_width = len(str(
      max(max_lines_linenr, max_regexps_linenr, max_comments_linenr)))

   skip = compute_initial_matching_sequence(regexps, lines)
   output(4, f'initial matching sequence length: {skip}')
   regexps = regexps[skip:]
   lines   = lines[skip:]

   lcs_table = compute_lcs_table(regexps, lines)
   output(4, f'lcs_table: {lcs_table}')

   counts = show_diffs_from_lcs_table(lcs_table, regexps, lines)
   for message in get_comments_output(tail_comments):
      print(message)

   if counts[1] > 0:
      output_diffs_summary(0, counts)
      if args.error:
         output(1, 'There were unmatched input lines, exiting with error.')
         sys.exit(1)
   elif counts[0] > 0:
      output_diffs_summary(0, counts)
      if args.error_unused:
         output(1, 'There were unmatched suppressions, exiting with error.')
         sys.exit(1)
   else:
      output_diffs_summary(1, counts) # only show in verbose mode
      output(1, 'Exiting successfully.')
      sys.exit(0)

if __name__ == "__main__":
   run_scafaps()
