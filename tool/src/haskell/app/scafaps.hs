-- Copyright (C) 2026 Dirk Herrmann

-- TODO: Use Data.Text as alternative to String?
-- probably unnecessary, string is already unicode.
-- Sample Unicode string: AöЖ€𝄞

-- TODO: Data.Vector / Data.Sequence as alternative to []?

-- TODO: Output of compiled regexps with out3 is not very user friendly, same
-- problem with Python code.

import Control.Monad (unless, when)
import Data.List (findIndex)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Vector ((!), constructN, fromList, Vector)
import Data.Version (showVersion)
import Data.Time.Clock.System (getSystemTime, systemNanoseconds, systemSeconds)
import GHC.IO.Handle (hGetContents, Handle)
import qualified Options.Applicative as Opts
import Paths_scafaps (version)
import System.Directory (doesFileExist)
import System.Exit (exitSuccess, exitWith, ExitCode(ExitFailure))
import System.IO (hClose, hPutStrLn, openFile, stderr, stdin, IOMode(..))
import Text.Printf (printf)
import qualified Text.Regex.TDFA as Rgx
import qualified Text.Regex.TDFA.ReadRegex as RdRgx

------------------------------------------------------------------------------
-- Verbosity controlled output types, functions and constants
------------------------------------------------------------------------------

type Verbosity = Int
type Level     = Int

maxVerbosity :: Verbosity
maxVerbosity = 5

prt :: Level -> Verbosity -> String -> IO ()
prt level verbosity string
  | verbosity >= maxVerbosity =
      putStrLn $ "Verbosity " ++ show level ++ ": " ++ string
  | verbosity >= level =
      putStrLn string
  | otherwise =
      return ()

prtErr :: String -> IO ()
prtErr s = hPutStrLn stderr s

prtVerbosityExplanation :: Verbosity -> IO ()
prtVerbosityExplanation v =
  let prefix = "Verbosity level " ++ show v ++ ": "
      prtExplanation :: String -> IO ()
      prtExplanation s = prt 1 v $ prefix ++ s
      maxv = show maxVerbosity
  in case v of
    0 -> return () -- Verbosity level 0: normal program output
    1 -> prtExplanation "user support (io, also show matches)"
    2 -> prtExplanation "user support (explain matching results)"
    3 -> prtExplanation "user support (debugging user input)"
    4 -> prtExplanation "dev debug (show internal results)"
    5 -> prtExplanation "dev debug (show verbosity levels)"
    _ -> prtExplanation $ "no such level, using " ++ maxv ++ " (max)"

------------------------------------------------------------------------------
-- Common input functions and types, and functions for reading input lines
-- subject to suppression
------------------------------------------------------------------------------

type RawLine = String
type LineNr  = Int

readRawLines :: Handle -> IO [RawLine]
readRawLines handle = do
  wholeFile <- hGetContents handle
  return $ lines wholeFile

enumerateRawLines :: [RawLine] -> [(LineNr, RawLine)]
enumerateRawLines rawLines = zip [1..] rawLines

data NumberedLine = NumberedLine {
    lineNr :: LineNr,
    content :: RawLine
  } deriving (Show)

getNumberedLines :: [RawLine] -> [NumberedLine]
getNumberedLines rawLines =
  let numberedLines = enumerateRawLines rawLines
      toLine (number, rawLine) =
        NumberedLine { lineNr = number, content = rawLine }
  in map toLine numberedLines

------------------------------------------------------------------------------
-- Input functions and types for reading suppressions
------------------------------------------------------------------------------

data CompiledRegexp = CompiledRegexp {
    sourceLine   :: NumberedLine,
    errStr       :: Maybe String,
    compiled     :: Rgx.Regex,
    comments     :: [NumberedLine]
  }

anchoredRegex :: RawLine -> String
anchoredRegex str =
  "^" ++ str ++ "$"

instance Show CompiledRegexp where
  show (CompiledRegexp (NumberedLine lnNr ctnt) err _cmpld cmts) =
    "Regexp {lineNr = " ++ show lnNr
    ++ ", content = " ++ show ctnt
    ++ ", error = " ++ show err
    ++ ", compiled(\"" ++ anchoredRegex ctnt ++ "\")"
    ++ ", comments = " ++ show cmts ++ "}"

-- Compiled "$a" is used as a regex that never matches.  Was tested against
-- the following input strings: "" "a" "$a" "$" "\na" "\ra" "\n\ra" "\r\na"
--
-- Note that we can use makeRegex instead of makeRegexM here: we truly want
-- the regex to compile successfully, if not, error is OK since we have to fix
-- the code.
neverMatches :: Rgx.Regex
neverMatches = Rgx.makeRegex "$a" :: Rgx.Regex

rgxCompOpts :: Rgx.CompOption
rgxCompOpts = Rgx.CompOption {
  Rgx.caseSensitive  = True,
  Rgx.multiline      = False,
  Rgx.rightAssoc     = True,
  Rgx.newSyntax      = False,
  Rgx.lastStarGreedy = True }

rgxExecOpts :: Rgx.ExecOption
rgxExecOpts = Rgx.ExecOption {
  Rgx.captureGroups  = False }

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
getCompiledRegexp :: LineNr -> RawLine -> [NumberedLine] -> CompiledRegexp
getCompiledRegexp nr str cmts =
  let parseResult = RdRgx.parseRegex str
      anchoredStr = anchoredRegex str
      regexM :: Maybe Rgx.Regex
      regexM = Rgx.makeRegexOptsM rgxCompOpts rgxExecOpts anchoredStr
      regex = case regexM of
        Just rx -> rx
        _ -> neverMatches
      errStrM = case (parseResult, regexM) of
        (Left e, _) -> Just $ show e
        (_, Nothing) -> Just $ "Offending ^ or $: >>" ++ anchoredStr ++ "<<"
        _ -> Nothing
  in CompiledRegexp {
    sourceLine   = NumberedLine nr str,
    errStr       = errStrM,
    compiled     = regex,
    comments     = cmts
  }

isComment :: RawLine -> Bool
isComment rawLine =
  not (null rawLine) && (head rawLine == '#')

getCompiledRegexpsHelper ::
  [(LineNr, RawLine)] -> [CompiledRegexp] -> [NumberedLine]
  -> ([CompiledRegexp], [NumberedLine])
-- End of list, just return what we have collected
getCompiledRegexpsHelper [] regexes cmts =
  (reverse regexes, reverse cmts)
-- Found a comment line, put it to our collection and move on
getCompiledRegexpsHelper ((rawLineNr,rawLine):xs) regexes cmts
  | isComment rawLine =
      let newComment = NumberedLine { lineNr = rawLineNr, content = rawLine }
      in getCompiledRegexpsHelper xs regexes $ newComment:cmts
-- Found a regexp line, create a CompiledRegex that also contains all the
-- comments found so far.  Then, continue but start afresh collecting
-- comments.
getCompiledRegexpsHelper ((rawLineNr,rawLine):xs) regexes cmts =
  let newRegexp = getCompiledRegexp rawLineNr rawLine (reverse cmts)
  in getCompiledRegexpsHelper xs (newRegexp:regexes) []

-- The input list of strings is a mixture of comment lines and regexp lines.
-- The comment lines are interpreted to belong to following regexp line and
-- are added to the structure for that compiled regexp.  At the end of the
-- list of strings can be some tail comments - these are returned separately,
-- thus the resulting tuple of compiled regexps and (numbered) comment lines
getCompiledRegexps :: [RawLine] -> ([CompiledRegexp], [NumberedLine])
getCompiledRegexps rawLines =
  let enumeratedLines = enumerateRawLines rawLines
  in getCompiledRegexpsHelper enumeratedLines [] []

prtErrRegexCompile :: CompiledRegexp -> IO ()
prtErrRegexCompile errorRegexp = do
  let srcLineNr = lineNr $ sourceLine errorRegexp
  prtErr $ "Error compiling suppression in line " ++ show srcLineNr ++ ":"
  prtErr $ fromJust $ errStr errorRegexp

readCompiledRegexps :: String -> IO ([CompiledRegexp], [NumberedLine], Int)
readCompiledRegexps supprFileName = do
  -- intentionally not using withFile below: it closes the file _before_
  -- subsequent lazy operations access the file contents.
  supprFileHandle <- openFile supprFileName ReadMode
  rawLines <- readRawLines supprFileHandle
  let (compiledRegexps, cmts) = getCompiledRegexps rawLines
  let errorRegexps = filter (isJust . errStr) compiledRegexps
  let errors = length errorRegexps
  mapM_ prtErrRegexCompile errorRegexps
  hClose supprFileHandle
  return (compiledRegexps, cmts, errors)

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
computeLcsTable regexps numberedLines =
  let regexpsV = fromList regexps        -- regexps as Vector
      linesV   = fromList numberedLines  -- lines as Vector
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

------------------------------------------------------------------------------
-- Types and functions to output matching results
------------------------------------------------------------------------------

data LineType =
  UnmatchedRegex   -- unmatched suppression regex
  | UnmatchedInput -- unmatched input line
  | MatchedRegex   -- matched suppression regex
  | MatchedInput   -- matched input line
  | CommentLine    -- comment line from suppression file

-- First character in the output is the line type
instance Show LineType where
  show UnmatchedRegex = "-"
  show UnmatchedInput = "+"
  show MatchedRegex   = "~"
  show MatchedInput   = "="
  show CommentLine    = "#"

type Width = Int

-- The central formatting function for result output.  The format is:
-- {lineTypeChar}{validityChar}{padded lineNr}: {content}
showLine :: Width -> LineType -> Bool -> NumberedLine -> String
showLine width lineType valid line =
  let lineTypeChar = show lineType
      validityChar = if valid then " " else "*"
      paddedLineNr = printf "%*d" width $ lineNr line
  in lineTypeChar ++ validityChar ++ paddedLineNr ++ ": " ++ (content line)

prtComment :: Verbosity -> Width -> NumberedLine -> IO ()
prtComment v width ln =
  prt 1 v $ showLine width CommentLine True ln

prtComments :: Verbosity -> Width -> [NumberedLine] -> IO ()
prtComments v width cmts =
  mapM_ (prtComment v width) cmts

prtMatch :: Verbosity -> Width -> (CompiledRegexp, NumberedLine) -> IO ()
prtMatch v width (rx, ln) = do
  prtComments v width $ comments rx
  prt 2 v "Match:"
  prt 1 v $ showLine width MatchedRegex True (sourceLine rx)
  prt 1 v $ showLine width MatchedInput True ln

prtIMS :: Verbosity -> Width -> [CompiledRegexp] -> [NumberedLine] -> Int -> IO ()
prtIMS v width rxs lns skip =
  mapM_ (prtMatch v width) $ take skip $ zip rxs lns

prtLcsDiff :: Verbosity -> Width -> [CompiledRegexp] -> [NumberedLine] ->
  Vector (Vector Int) -> IO (Int, Int)
prtLcsDiff v width regexes lines lcsTable =
  return (0, 0) -- TODO

prtTailComments :: Verbosity -> Width -> [NumberedLine] -> IO ()
prtTailComments v width cmts =
  return () -- TODO

prtDiffSummary :: Verbosity -> (Int,Int) -> IO ()
prtDiffSummary v counts =
  return () -- TODO

prtResults ::
  Verbosity -> [CompiledRegexp] -> [NumberedLine] ->
  Int -> Vector (Vector Int) -> [NumberedLine] ->
  IO (Int, Int)
prtResults v regexes lines lIMS lcsTable cmts = do
  let maxLinesLineNr = if null lines then 0 else lineNr $ last lines
      maxRegexpsLineNr =
        if null regexes then 0 else lineNr $ sourceLine $ last regexes
      maxTailCommentsLineNr = if null cmts then 0 else lineNr $ last cmts
      maxLineNr = max maxLinesLineNr $ max maxRegexpsLineNr maxTailCommentsLineNr
      width = length $ show maxLineNr
  prtIMS v width regexes lines lIMS
  counts <- prtLcsDiff v width regexes lines lcsTable
  prtTailComments v width cmts
  prtDiffSummary v counts
  return counts

copyInputToOutput :: Handle -> IO ()
copyInputToOutput handle = do
  wholeFile <- hGetContents handle
  putStr wholeFile

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
    optVerbosity :: Verbosity
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

getSystemTimeAsFloat :: IO Double
getSystemTimeAsFloat = do
  t <- getSystemTime
  let s = fromIntegral (systemSeconds t) :: Double
      n = fromIntegral (systemNanoseconds t) :: Double
  return (s + 0.000000001 * n)

main :: IO ()
main = do
  -- BEGIN Performance measurement
  t0 <- getSystemTimeAsFloat
  -- END   Performance measurement
  -- read options
  options <- Opts.execParser cmdLineOptionsGrammar

  -- handling verbosity
  let v = optVerbosity options
  -- BEGIN Performance measurement
  prt 4 v $ printf "TIMESTAMP t0: %20.9f                  start" t0
  -- END   Performance measurement
  prtVerbosityExplanation v
  prt 3 v $ show options

  -- BEGIN Performance measurement
  t1 <- getSystemTimeAsFloat
  prt 4 v $ printf "TIMESTAMP t1: %20.9f  " t1
    ++ printf "delta: %7.3f  read options" (t1 - t0)
  -- END   Performance measurement

  -- read suppressions
  let supprFileName = optFileName options
  supprFileExists <- doesFileExist supprFileName
  let onFileNotFound = optWhatIfFileNotFound options
  let fnfMsg = "Suppressions-file '" ++ supprFileName ++ "' not found"
  (compiledRegexps, tailCmts) <-
    case (supprFileExists, onFileNotFound) of
      (True, _) -> do
        prt 1 v $ "Reading suppressions from '" ++ supprFileName ++ "'"
        (compiledRegexps, cmts, errors) <- readCompiledRegexps supprFileName
        when (errors > 0) $ do
          prtErr $ "Compilation errors in " ++ show errors ++ " suppressions"
          unless (optKeepGoingWithCompileError options) $ do
            exitWith $ ExitFailure 1
        prt 3 v $ "Suppression regexps: " ++ show compiledRegexps
        return (compiledRegexps, cmts)
      (False, FnfError) -> do
        prtErr $ "Error: " ++ fnfMsg
        exitWith $ ExitFailure 1
      (False, FnfEmpty) -> do
        prt 1 v $ fnfMsg ++ ", treating it as an empty file"
        return ([], [])
      (False, FnfPass) -> do
        -- TODO: Currently, the lines that are subject to suppression can only
        -- be provided via stdin.  In future the possibility may be added to
        -- give a file name alternatively to stdin.  Then, for the case that a
        -- file name is given, it has to be re-considered what the purpose of
        -- the pass option would be.
        prt 1 v $ fnfMsg ++ ", passing input data through"
        copyInputToOutput stdin
        exitSuccess
  -- BEGIN Performance measurement
  t2 <- getSystemTimeAsFloat
  prt 4 v $ printf "TIMESTAMP t2: %20.9f  " t2
    ++ printf "delta: %7.3f  read suppressions" (t2 - t1)
  -- END   Performance measurement

  -- read input lines subject to suppression
  prt 1 v "Reading input lines (SCA output) from stdin"
  rawLines <- readRawLines stdin
  let numberedLines = getNumberedLines rawLines
  prt 3 v $ "Input lines: " ++ show numberedLines
  -- BEGIN Performance measurement
  t3 <- getSystemTimeAsFloat
  prt 4 v $ printf "TIMESTAMP t3: %20.9f  " t3
    ++ printf "delta: %7.3f  read input lines" (t3 - t2)
  -- END   Performance measurement

  let lenInitMatchingSeq =
        lengthOfInitialMatchingSequence compiledRegexps numberedLines
  prt 1 v $ "Length of initial matching sequence: " ++ show lenInitMatchingSeq
  -- BEGIN Performance measurement
  t4 <- getSystemTimeAsFloat
  prt 4 v $ printf "TIMESTAMP t4: %20.9f  " t4
    ++ printf "delta: %7.3f  compute initial matching sequence" (t4 - t3)
  -- END   Performance measurement

  let remainingRegexps = drop lenInitMatchingSeq compiledRegexps
      remainingLines   = drop lenInitMatchingSeq numberedLines
      lcsTable = computeLcsTable remainingRegexps remainingLines
  prt 4 v $ "lcsTable = " ++ show lcsTable
  -- BEGIN Performance measurement
  t5 <- getSystemTimeAsFloat
  prt 4 v $ printf "TIMESTAMP t5: %20.9f  " t5
    ++ printf "delta: %7.3f  computed lcstable" (t5 - t4)
  -- END   Performance measurement

  counts <-
    prtResults v compiledRegexps numberedLines lenInitMatchingSeq lcsTable tailCmts
  -- BEGIN Performance measurement
  t6 <- getSystemTimeAsFloat
  prt 4 v $ printf "TIMESTAMP t6: %20.9f  " t6
    ++ printf "delta: %7.3f  show results" (t6 - t5)
  -- END   Performance measurement

  when ((snd counts > 0) && optErrorOnUnsuppressedInput options) $ do
    prtErr "There were unmatched input lines, exiting with error."
    exitWith $ ExitFailure 1
  when ((fst counts > 0) && optErrorOnUnusedSuppressions options) $ do
    prtErr "There were unmatched suppressions, exiting with error."
    exitWith $ ExitFailure 1
  prt 1 v "Exiting successfully."
  exitSuccess
