-- Copyright (C) 2026 Dirk Herrmann

-- TODO: Use Data.Text as alternative to String?
-- probably unnecessary, string is already unicode.
-- Sample Unicode string: AöЖ€𝄞

-- TODO: Data.Vector / Data.Sequence as alternative to []?

-- TODO: Output of compiled regexps with out3 is not very user friendly, same
-- problem with Python code.

import Control.Monad (unless, when)
import Data.List (findIndex)
import Data.Maybe (fromMaybe)
import Data.Vector ((!), constructN, fromList, Vector)
import Data.Version (showVersion)
import GHC.IO.Handle (hGetContents, Handle)
import qualified Options.Applicative as Opts
import Paths_scafaps (version)
import System.Directory (doesFileExist)
import System.Exit (exitSuccess, exitWith, ExitCode(ExitFailure))
import System.IO (
  hClose, hPutStr, hPutStrLn, openFile, stderr, stdin, stdout, IOMode(..))
import qualified Text.Regex.TDFA as Rgx

------------------------------------------------------------------------------
-- Verbosity controlled output functions and constants
------------------------------------------------------------------------------

maxVerbosity :: Int
maxVerbosity = 5

explainVerbosity :: Int -> IO ()
explainVerbosity v =
  let prefix = "Verbosity level " ++ show v ++ ": "
      pubVerbInfoLn :: String -> IO ()
      pubVerbInfoLn s = putStrLn $ prefix ++ s
      maxv = show maxVerbosity
  in case v of
    0 -> return () -- Verbosity level 0: normal program output
    1 -> pubVerbInfoLn "user support (io, also show matches)"
    2 -> pubVerbInfoLn "user support (explain matching results)"
    3 -> pubVerbInfoLn "user support (debugging user input)"
    4 -> pubVerbInfoLn "dev debug (show internal results)"
    5 -> pubVerbInfoLn "dev debug (show verbosity levels)"
    _ -> pubVerbInfoLn $ "no such level, using " ++ maxv ++ " (max)"

-- Text shall only once go through the verbosity check.  Otherwise, in case of
-- maxVerbosity, the level may get added twice.  Thus, output that is ready
-- for the check gets type OutputLine.
data OutputLine =
  OutputString Int String

output :: Int -> OutputLine -> IO ()
output verbosity (OutputString level string)
  | verbosity >= maxVerbosity =
      putStrLn $ "Verbosity " ++ show level ++ ": " ++ string
  | verbosity >= level =
      putStrLn string
  | otherwise =
      return ()

mkOutput :: Int -> String -> OutputLine
mkOutput level string =
  OutputString level string

errout :: String -> IO ()
errout s = hPutStrLn stderr s

------------------------------------------------------------------------------
-- Common input functions and types, and functions for reading input lines
-- subject to suppression
------------------------------------------------------------------------------

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

------------------------------------------------------------------------------
-- Input functions and types for reading suppressions
------------------------------------------------------------------------------

data CompiledRegexp = CompiledRegexp {
    sourceLineNr :: Integer,
    source       :: String,
    valid        :: Bool,
    compiled     :: Rgx.Regex,
    comments     :: [NumberedLine]
  }

instance Show CompiledRegexp where
  show (CompiledRegexp lnr src val _cmpld comm) =
    "Regexp {lineNr = " ++ show lnr ++ ", content = " ++ show src
    ++ ", valid = " ++ show val ++ ", compiled(\"^" ++ src ++ "$\")"
    ++ ", comments = " ++ show comm ++ "}"

-- Compiled "$a" is used as a regex that never matches.  Was tested against
-- the following input strings: "" "a" "$a" "$" "\na" "\ra" "\n\ra" "\r\na"
--
-- Note that we can use makeRegex instead of makeRegexM here: we truly want
-- the regex to compile successfully, if not, error is OK since we have to fix
-- the code.
neverMatches :: Rgx.Regex
neverMatches = Rgx.makeRegex "$a" :: Rgx.Regex

-- For a line that is expected to contain a valid regexp, we create the
-- CompiledRegexp structure.  As input we have the line number, the raw line
-- that forms the source of regexp compilation and the collected comment lines
-- that precede the regexp in the suppressions file.
--
-- The regexp is considered to match a line if it is a full match, from the
-- beginning of the line to the end.  This can be achived in several ways.
-- One option (A) is to enclose the regexp string between "^" and "$".
-- Another option (B) is to use the match overload that delivers a result of
-- type (Rgx.MatchOffset, Rgx.MatchLength) and check if Rgx.MatchLength equals
-- the length of the string to be matched.  This works because TDFA implements
-- POSIX matching which requires to deliver the longest match.
--
-- Option A has some advantages over B: First, it is faster, because a) the
-- regexp engine handles the anchored regexp better, and b) because we can use
-- a match overload that only delivers a Bool result, and c) we don't have to
-- compare the result against the length of the string to be matched.  Second,
-- the regexp compiler does not accept empty regexps, but an empty string
-- becomes "^$", which is accepted by the compiler.  The only problem with
-- option A is, that it will fail if the regexp string argument already
-- contains one of these anchors.
getCompiledRegexp :: Integer -> String -> [NumberedLine] -> CompiledRegexp
getCompiledRegexp nr str commnts =
  let srcStr = "^" ++ str ++ "$"
      compOpts = Rgx.CompOption {
        Rgx.caseSensitive  = True,
        Rgx.multiline      = False,
        Rgx.rightAssoc     = True,
        Rgx.newSyntax      = False,
        Rgx.lastStarGreedy = True }
      execOpts = Rgx.ExecOption {
        Rgx.captureGroups  = False }
      -- to avoid "error" being called for bad regexps use makeRegexOptsM with
      -- Maybe: if the regex can be compiled, we get 'Just Regex', else
      -- 'Nothing'.  In case of succcess, the regex has to be extracted from
      -- the Maybe.
      regexM = Rgx.makeRegexOptsM compOpts execOpts srcStr :: Maybe Rgx.Regex
  in CompiledRegexp {
    sourceLineNr = nr,
    source       = str, -- use this for printing the original line
    valid        = case regexM of
        Just _  -> True
        Nothing -> False,
    compiled     = fromMaybe neverMatches regexM,
    comments     = commnts
  }

isComment :: [Char] -> Bool
isComment rawLine =
  not (null rawLine) && (head rawLine == '#')

getCompiledRegexpsHelper ::
  [(Integer, String)] -> [CompiledRegexp] -> [NumberedLine]
  -> ([CompiledRegexp], [NumberedLine])
-- End of list, just return what we have collected
getCompiledRegexpsHelper [] regexes commnts =
  (reverse regexes, reverse commnts)
-- Found a comment line, put it to our collection and move on
getCompiledRegexpsHelper ((rawLineNr,rawLine):xs) regexes commnts
  | isComment rawLine =
      let newComment = NumberedLine { lineNr = rawLineNr, content = rawLine }
      in getCompiledRegexpsHelper xs regexes $ newComment:commnts
-- Found a regexp line, create a CompiledRegex that also contains all the
-- comments found so far.  Then, continue but start afresh collecting
-- comments.
getCompiledRegexpsHelper ((rawLineNr,rawLine):xs) regexes commnts =
  let newRegexp = getCompiledRegexp rawLineNr rawLine (reverse commnts)
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

erroutRegexErr :: CompiledRegexp -> IO ()
erroutRegexErr errorRegexp = do
  let srcLineNr = sourceLineNr errorRegexp
  let supprSrc = source errorRegexp
  errout $ "Error compiling suppression in line " ++ show srcLineNr ++ ":"
  errout $ "  Offending suppression: >>" ++ supprSrc ++ "<<"

readCompiledRegexps :: String -> IO ([CompiledRegexp], [NumberedLine], Int)
readCompiledRegexps supprFileName = do
  -- intentionally not using withFile below: it closes the file _before_
  -- subsequent lazy operations access the file contents.
  supprFileHandle <- openFile supprFileName ReadMode
  rawLines <- readRawLines supprFileHandle
  let (compiledRegexps, commnts) = getCompiledRegexps rawLines
  let errorRegexps = filter (not . valid) compiledRegexps
  let errors = length errorRegexps
  mapM_ erroutRegexErr errorRegexps
  hClose supprFileHandle
  return (compiledRegexps, commnts, errors)

------------------------------------------------------------------------------
-- Functions to compute the longest common subsequence (LCS)
------------------------------------------------------------------------------

isFullMatch :: CompiledRegexp -> NumberedLine -> Bool
isFullMatch rx ln =
  Rgx.match (compiled rx) (content ln) :: Bool

lengthOfInitialMatchingSequence :: [CompiledRegexp] -> [NumberedLine] -> Int
lengthOfInitialMatchingSequence regexps numberedLines =
  let zipped = zip regexps numberedLines
      isNotFullMatch (rx,ln) = not $ isFullMatch rx ln
      maybeFirstMismatch = findIndex isNotFullMatch zipped
  in fromMaybe (length zipped) maybeFirstMismatch

-- Scafaps, similar to diff, computes the LCS.  In contrast to diff, we don't
-- compare the lines for equality, but instead see if the respective line is
-- fully matched by the corresponding regular expression.  For an explanation
-- of the LCS algorithm, see Wikipedia.
--
-- Some remarks:
-- * To simplify the algorithm, the table is extended by a column 0 and a row
--   0, which are filled with zeroes.  This requires the table to be enlarged,
--   see the size arguments of the calls to constructN.
-- * To obtain a two-dimensional vector, two calls to constructN are nested.
--   A call to constructN represents a loop - the given size argument is the
--   size of the resulting vector and determines the number of iterations, and
--   the given function corresponds to the loop body.
computeLcsTable :: [CompiledRegexp] -> [NumberedLine] -> Vector (Vector Int)
computeLcsTable regexps lines =
  let regexpsV = fromList regexps -- regexps as Vector
      linesV   = fromList lines   -- lines as Vector
      innerLoopBody :: Vector (Vector Int) -> Vector Int -> Int
      innerLoopBody lcsTmp rowTmp =
        let i = length lcsTmp
            j = length rowTmp
            rx = regexpsV ! (i - 1) -- lazy, otherwise error with i==0
            ln = linesV ! (j - 1)   -- lazy, otherwise error with j==0
            result
              | i == 0 = 0
              | j == 0 = 0
              | isFullMatch rx ln = (lcsTmp ! (i-1) ! (j-1)) + 1
              | otherwise = max (rowTmp ! (j-1)) (lcsTmp ! (i-1) ! j)
        in result
      outerLoopBody :: Vector (Vector Int) -> Vector Int
      outerLoopBody lcsTmp
        = constructN (1 + length linesV) $ innerLoopBody lcsTmp
  in constructN (1 + length regexpsV) outerLoopBody

copyInputToOutput :: Handle -> IO ()
copyInputToOutput handle = do
  wholeFile <- hGetContents handle
  hPutStr stdout wholeFile

------------------------------------------------------------------------------
-- Functions and types for command line parsing
------------------------------------------------------------------------------

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

------------------------------------------------------------------------------
-- main
------------------------------------------------------------------------------

main :: IO ()
main = do
  -- read options
  options <- Opts.execParser cmdLineOptionsGrammar

  -- handling verbosity
  let verbosity = optVerbosity options
  explainVerbosity verbosity
  let out0 s = output verbosity $ mkOutput 0 s
  let out1 s = output verbosity $ mkOutput 1 s
  let out3 s = output verbosity $ mkOutput 3 s
  let out4 s = output verbosity $ mkOutput 4 s
  out3 $ show options

  -- read suppressions
  let supprFileName = optFileName options
  supprFileExists <- doesFileExist supprFileName
  let onFileNotFound = optWhatIfFileNotFound options
  let fnfMsg = "Suppressions-file '" ++ supprFileName ++ "' not found"
  (compiledRegexps, commnts) <-
    case (supprFileExists, onFileNotFound) of
      (True, _) -> do
        out1 $ "Reading suppressions from '" ++ supprFileName ++ "'"
        (compiledRegexps, commnts, errors) <- readCompiledRegexps supprFileName
        when (errors > 0) $ do
          errout $ "Compilation errors in " ++ show errors ++ " suppressions"
          unless (optKeepGoingWithCompileError options) $ do
            exitWith $ ExitFailure 1
        out3 $ "Suppression regexps: " ++ show compiledRegexps
        return (compiledRegexps, commnts)
      (False, FnfError) -> do
        errout $ "Error: " ++ fnfMsg
        exitWith $ ExitFailure 1
      (False, FnfEmpty) -> do
        out1 $ fnfMsg ++ ", treating it as an empty file"
        return ([], [])
      (False, FnfPass) -> do
        -- TODO: Currently, the lines that are subject to suppression can only
        -- be provided via stdin.  In future the possibility may be added to
        -- give a file name alternatively to stdin.  Then, for the case that a
        -- file name is given, it has to be re-considered what the purpose of
        -- the pass option would be.
        out1 $ fnfMsg ++ ", passing input data through"
        copyInputToOutput stdin
        exitSuccess

  -- read input lines subject to suppression
  out1 "Reading input lines (SCA output) from stdin"
  rawLines <- readRawLines stdin
  let lines = getNumberedLines rawLines
  out3 $ "Input lines: " ++ show lines

  let lenInitMatchingSeq =
        lengthOfInitialMatchingSequence compiledRegexps lines
  out1 $ "Length of initial matching sequence: " ++ show lenInitMatchingSeq

  let lcsTable = computeLcsTable compiledRegexps lines
  out4 $ "lcsTable = " ++ show lcsTable

  return ()
