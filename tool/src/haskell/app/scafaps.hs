-- Copyright (C) 2026 Dirk Herrmann

-- TODO: Use Data.Text as alternative to String?
-- probably unnecessary, string is already unicode.
-- Sample Unicode string: AöЖ€𝄞

-- TODO: Data.Vector / Data.Sequence as alternative to []?

import Control.Monad (when)
import Data.Maybe (fromMaybe)
import Data.Version (showVersion)
import GHC.IO.Handle (hGetContents, Handle)
import qualified Options.Applicative as Opts
import Paths_scafaps (version)
import System.IO (stdin)
import qualified Text.Regex.TDFA as Rgx

maxVerbosity :: Int
maxVerbosity = 5

explainVerbosity :: Int -> IO ()
explainVerbosity v =
  let prefix = "Verbosity level " ++ show v ++ ": "
      maxv   = show maxVerbosity
  in case v of
    0 -> return () -- Verbosity level 0: normal program output
    1 -> putStrLn $ prefix ++ "user support (io, also show matches)"
    2 -> putStrLn $ prefix ++ "user support (explain matching results)"
    3 -> putStrLn $ prefix ++ "user support (debugging user input)"
    4 -> putStrLn $ prefix ++ "dev debug (show internal results)"
    5 -> putStrLn $ prefix ++ "dev debug (show verbosity levels)"
    _ -> putStrLn $ prefix ++ "no such level, using " ++ maxv ++ " (max)"

-- Text shall only once go through the verbosity check.  Otherwise, the level
-- may get added twice.  Thus, text that has already gone through the check
-- gets type OutputLine.  OutputLine is intentionally not deriving Show!
newtype OutputLine = OutputLine {
    outString :: String
  }

output :: Maybe OutputLine -> IO ()
output (Just (OutputLine s)) =
  putStrLn s
output Nothing =
  return ()

mkOutput :: Show a => Int -> Int -> a -> Maybe OutputLine
mkOutput verbosity level a
  | verbosity >= maxVerbosity =
    Just $ OutputLine $ "Verbosity " ++ show level ++ ": " ++ show a
  | verbosity >= level =
    Just $ OutputLine $ show a
  | otherwise =
    Nothing

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

getVersion :: String
getVersion =
  "scafaps " ++ showVersion version

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

cmdLineOptionsGrammarPart :: Opts.Parser Options
cmdLineOptionsGrammarPart =
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

cmdLineOptionsGrammar :: Opts.ParserInfo Options
cmdLineOptionsGrammar =
  Opts.info
    ( cmdLineOptionsGrammarPart
      Opts.<**> Opts.simpleVersioner getVersion
      Opts.<**> Opts.helper )
    ( Opts.fullDesc
    <> Opts.header "scafaps - a tool to suppress from static code analysis" )

main :: IO ()
main = do
  -- read options
  options <- Opts.execParser cmdLineOptionsGrammar
  let verbosity = optVerbosity options
  explainVerbosity verbosity
  output $ mkOutput verbosity 3 options

  rawLines <- readRawLines stdin
  let (compiledRegexps, commnts) = getCompiledRegexps rawLines
      toCompiledRegexpStr rx =
        let flag = if valid rx then " " else "*"
        in show (sourceLineNr rx) ++ ":" ++ flag ++ (source rx) 
  mapM_ ( putStrLn . toCompiledRegexpStr ) compiledRegexps
