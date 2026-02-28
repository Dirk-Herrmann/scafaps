-- Copyright (C) 2026 Dirk Herrmann

-- TODO: Use Data.Text as alternative to String?
-- probably unnecessary, string is already unicode.
-- Sample Unicode string: AöЖ€𝄞

-- TODO: Data.Vector / Data.Sequence as alternative to []?

import Data.Maybe (fromMaybe)
import GHC.IO.Handle (hGetContents, Handle)
import qualified Options.Applicative as Opts
import System.IO (stdin)
import qualified Text.Regex.TDFA as Rgx

-- 
-- 
-- output :: String -> IO ()
-- output s = print s
-- 
-- output :: Maybe String -> IO ()
-- output Nothing = return
-- output Just s = output s

readRawLines :: Handle -> IO [String]
readRawLines handle = do
  wholeFile <- hGetContents handle
  return $ lines wholeFile

enumerateRawLines :: [String] -> [(Integer, String)]
enumerateRawLines rawLines = zip [1..] rawLines

data NumberedLine = NumberedLine {
    lineNr :: Integer,
    content :: String
  } deriving (Show)

getNumberedLines :: [String] -> [NumberedLine]
getNumberedLines rawLines =
  let numberedLines = enumerateRawLines rawLines
      toLine (number, rawLine) =
        NumberedLine { lineNr = number, content = rawLine }
  in map toLine numberedLines

data CompiledRegexp = CompiledRegexp {
    sourceLineNr :: Integer,
    source       :: String,
    valid        :: Bool,
    compiled     :: Rgx.Regex,
    comments     :: [NumberedLine]
  } -- deriving (Show)

-- Compiled "$a" is used as a regex that never matches.  Was tested against
-- the following input strings: "" "a" "$a" "$" "\na" "\ra" "\n\ra" "\r\na"
--
-- Note that we can use makeRegex instead of makeRegexM here: we truly want
-- the regex to compile successfully, if not, error is OK since we have to fix
-- the code.
neverMatches :: Rgx.Regex
neverMatches = Rgx.makeRegex "$a" :: Rgx.Regex

-- The regex compiler does not accept an empty string.  In our case, the empty
-- string is for an empty line.
matchesEmptyLine :: Rgx.Regex
matchesEmptyLine = Rgx.makeRegex "^$" :: Rgx.Regex

-- For a line that is expected to contain a valid regexp, we create the
-- CompiledRegexp structure.  As input we have the line number, the raw line
-- that forms the source of regexp compilation and the collected comment lines
-- that precede the regexp in the suppressions file.
getCompiledRegexp :: Integer -> String -> [NumberedLine] -> CompiledRegexp
getCompiledRegexp nr str commnts =
  -- to avoid "error" being called for bad regexps use makeRegexM with Maybe:
  -- if the regex can be compiled, we get 'Just Regex', else 'Nothing'.  In
  -- case of succcess, the regex has to be extracted from the Maybe.
  let regexM = if null str  -- compiler does not accept empty lines
        then Just matchesEmptyLine
        else Rgx.makeRegexM str :: Maybe Rgx.Regex
  in CompiledRegexp {
    sourceLineNr = nr,
    source       = str,
    valid        = case regexM of
        Just _  -> True
        Nothing -> False,
    compiled     = fromMaybe neverMatches regexM,
    comments     = commnts
  }

isComment :: [Char] -> Bool
isComment rawLine
  = not (null rawLine) && (head rawLine == '#')

getCompiledRegexpsHelper ::
  [(Integer, String)] -> [CompiledRegexp] -> [NumberedLine]
  -> ([CompiledRegexp], [NumberedLine])
-- End of list, just return what we have collected
getCompiledRegexpsHelper [] regexes commnts
  = (reverse regexes, reverse commnts)
-- Found a comment line, put it to our collection and move on
getCompiledRegexpsHelper
    ((rawLineNr,rawLine):xs) regexes commnts | isComment rawLine
  = let newComment = NumberedLine { lineNr = rawLineNr, content = rawLine }
    in getCompiledRegexpsHelper xs regexes $ newComment:commnts
-- Found a regexp line, create a CompiledRegex that also contains all the
-- comments found so far.  Then, continue but start afresh collecting
-- comments.
getCompiledRegexpsHelper ((rawLineNr,rawLine):xs) regexes commnts
  = let newRegexp = getCompiledRegexp rawLineNr rawLine (reverse commnts)
    in getCompiledRegexpsHelper xs (newRegexp:regexes) []

-- The input list of strings is a mixture of comment lines and regexp lines.
-- The comment lines are interpreted to belong to following regexp line and
-- are added to the structure for that compiled regexp.  At the end of the
-- list of strings can be some tail comments - these are returned separately,
-- thus the resulting tuple of compiled regexps and (numbered) comment lines
getCompiledRegexps :: [String] -> ([CompiledRegexp], [NumberedLine])
getCompiledRegexps rawLines =
  let enumeratedLines = enumerateRawLines rawLines
  in getCompiledRegexpsHelper enumeratedLines [] []

-- FIXME: For Later:
-- -- Checks if the rx matches the beginning(!) of the string
-- match rx "zyxwvutsrqponml" :: Bool
-- -- Show the start and length of match.  TDFA matches are POSIX matches and
-- -- POSIX requires to delivery the longest match.  Thus, to check for a full
-- -- match, it is sufficient to check if MatchLength equals the length of the
-- -- string.
-- match rx "zyxwvutsrqponml" :: (MatchOffset, MatchLength)

data OnFileNotFound =
  FnfError | FnfEmpty | FnfPass
  deriving (Show)

toOnFileNotFound :: String -> Maybe OnFileNotFound
toOnFileNotFound "error" = Just FnfError
toOnFileNotFound "empty" = Just FnfEmpty
toOnFileNotFound "pass"  = Just FnfPass
toOnFileNotFound _       = Nothing

data Options = Options {
    optFileName :: String,
    optWhatIfFileNotFound :: OnFileNotFound,
    optKeepGoingWithCompileError :: Bool,
    optErrorOnUnsuppressedInput :: Bool,
    optErrorOnUnusedSuppressions :: Bool,
    optVerbosity :: Int
  } deriving (Show)

optionsParserSetup :: Opts.Parser Options
optionsParserSetup =
  Options
  <$> Opts.strArgument (
    Opts.metavar "suppressions-file"
    <> Opts.help "file with suppressions to be applied" )
  <*> Opts.option (Opts.maybeReader toOnFileNotFound) (
    Opts.long "suppressions-file-not-found"
    <> Opts.metavar "{error,empty,pass}"
    <> Opts.value FnfError
    <> Opts.help "how to proceed if the suppressions-file does not exist.  \
                 \If \"error\" is selected (the default), scafaps will exit \
                 \with an error code.  With \"empty\", scafaps will behave \
                 \as if an empty suppressions-file was given.  With \"pass\",\
                 \stdin will be copied directly to stdout as if scafaps was \
                 \not there." )
  <*> Opts.switch (
    Opts.long "keep-going-with-compile-errors"
    <> Opts.short 'k'
    <> Opts.help "keep going even if there are errors when compiling \
                 \suppressions" )
  <*> Opts.switch (
    Opts.long "error"
    <> Opts.short 'e'
    <> Opts.help "exit with error if unsuppressed output remains" )
  <*> Opts.switch (
    Opts.long "error-unused"
    <> Opts.short 'u'
    <> Opts.help "exit with error if there are unused suppressions" )
  <*> (length <$> Opts.many (Opts.flag' () (
    Opts.long "verbose"
    <> Opts.short 'v'
    <> Opts.help "increase verbosity level.  The option can be given several \
                 \times" )))

optionsParser :: Opts.ParserInfo Options
optionsParser =
  Opts.info
    ( optionsParserSetup
      Opts.<**> Opts.simpleVersioner "TODO: Show Version"
      Opts.<**> Opts.helper )
    ( Opts.fullDesc
    <> Opts.header "scafaps - a tool to suppress from static code analysis" )

main :: IO ()
main = do
  options <- Opts.execParser optionsParser
  print options
  rawLines <- readRawLines stdin
  let (compiledRegexps, commnts) = getCompiledRegexps rawLines
      toCompiledRegexpStr rx =
        let flag = if valid rx then " " else "*"
        in show (sourceLineNr rx) ++ ":" ++ flag ++ (source rx) 
  mapM_ ( putStrLn . toCompiledRegexpStr ) compiledRegexps
