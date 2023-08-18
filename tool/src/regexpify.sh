#!/bin/bash

# with errexit, to execute a command that is allowed to fail, we can prefix it
# with "!" - this is used to invert the return value of a command, but will
# also implies that the shell will not exit even if the command fails.
set -o errexit -o pipefail -o noclobber -o nounset

# not implemented yet:
#
# --blanks: substitute whitespace into more general whitespace.  This is a bit
#   tricky, because blanks within strings should remain untouched.  Moreover,
#   single blanks between two characters which - when joined - might result in
#   a new identifier, should never be eliminated.  For example: "a b" should
#   not be replaced by "a *b" to ensure that "ab" does not match.  It may be
#   replaced by "a +b", however, allowing additional blanks to be added.
#   Whitespace delimited by some operator or the like may possibly be
#   completely eliminated: "a * b" could be replaced by "a *[*] *b", thus
#   allowing to match also "a*b".  But - this can even depend on the
#   programming language.  In Scheme, for example, a dash is a valid part of
#   identifiers.  Here, "a - b" should not be transformed such that "a-b"
#   would match.
# --comments: would be nice to substitute comments automatically by regexps
#   that accept all comments.  Comments are programming language specific,
#   though, and we would probably have to limit the handling to single-line
#   comments.
# --paths: This should be easy, although we would probably limit the paths to
#   be substituted to UN*X paths (no windows) where the file and directory
#   names are sane (no blanks and other strange characters).
# --strings: Would be nice to be able to replace concrete string content with
#   regexps that match any string content.  Could be tricky due to quoting
#   etc.
# --wrnlvl: To replace, like, ERROR or WARN into something generic, thus
#   preparing for the case that a sca tool re-classifies a warning.
#
# Simpler to achieve could be to provide the user with the possibility to use
# shortcuts like %comment% within a comment.  This may even be done in a
# fairly language agnostic way if the shortcut %commment% would only be used
# for the content of the comment.  A C++ comment "/* arbitrary text +-* */"
# would then be replaceable by "/*%comment%*/".  Then, there would be no need
# to parse comments by the tool.
#
# To allow such shortcuts, any '%' in the original text would have to be
# quoted: s/%/[%]/g;
#
# Possible shortcuts and their names:
# - [0-9]+ : %num%, %number%
# - ^[[:blank:]]+ : %ind%, %indent%, %indentation%

function print_help () {
   cat <<-EOF
	usage: $0 [-h|--help] [-i|--indent] [-n|--nums]

	Convert lines read from stdin line by line into regular expressions which
	would match exactly those input lines.  Additional options can be given
	to have the resulting regexps match also lines that vary in some ways.

	Options:
	  -h, --help          show this help message and exit
	  -i, --indent        convert whitespace at beginning of lines into a regexp
	                      to match the line also with different indentation
	  -n, --nums          convert literal numbers into regexps matching any number

	EOF
}

# parse command line options
# this is done using getopt to get options in uniform way

# check if the enhanced version of getopt is installed
! getopt --test > /dev/null # will exit with 4 for enhanced getopt
if [[ ${PIPESTATUS[0]} -ne 4 ]]; then
   echo 'enhanced getopt required but not found in PATH.'
   exit 1
fi

long_options=help,indent,nums
short_options=hin
getopt_output=$(getopt --name "$0" --longoptions "${long_options}" --options "${short_options}" -- "$@")

eval set -- "${getopt_output}" # re-assign the positional arguments

# option defaults
indent=
nums=

while true; do
   case "$1" in
      '-h'|'--help')
         print_help
         exit 0
         ;;
      '-i'|'--indent')
         indent='s/^[[:blank:]]+/[ \t]+/;'
         shift
         ;;
      '-n'|'--nums')
         nums='s/[0-9]+/[0-9]+/g;'
         shift
         ;;
      '--')
         shift
         break
         ;;
      *)
         echo "$0: assertion failure"
         exit 2
         ;;
   esac
done

# positional arguments
if [[ $# -ne 0 ]]; then
   echo "$0: Unexpected options: $*"
   exit 1
fi

# s/<POSIX ERE regexp>/<Python regexp>/
sed --regexp-extended '
   s/\\/\\\\/g;
   s/([.*+?[(){^$])/\\&/g' \
| sed --regexp-extended "
   ${indent}
   ${nums} "
