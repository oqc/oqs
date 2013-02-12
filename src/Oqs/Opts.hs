module Opts (
  Options
, GrpStyle (RefRangesOnly, BigBreaks, AllBreaks, ByVerse)
, optVerbose
, optQpfFile
, optBrkStyle
, optGrpStyle
, optsParser
, printUsageInfo
) where

import System.Console.GetOpt
import System
import Control.Monad ( when )
import IO
import List ( intersperse )


-- EXPOSED


-- Intra-text grouping style (what to align in case of side-by-side)
data GrpStyle = RefRangesOnly  -- only group on ranges (least grouping)
              | BigBreaks      -- on ranges and big breaks
              | AllBreaks      -- on ranges and all breaks
              | ByVerse        -- on individual verses (most grouping)
                deriving Show

data Options = Options
  { optVerbose  :: Bool
  , optQpfFile  :: String
  , optBrkStyle :: Bool
  , optGrpStyle :: GrpStyle
  } deriving Show

optsParser :: [String] -> IO (Options, String, [String])
optsParser args =
  case getOpt RequireOrder options args of
    (o, refStr:files, []) -> do
       opts <- foldl (>>=) (return defaultOptions) o
       {- let Options { optVerbose  = verbose-}
       {-             , optQpfFile  = qpfFile-}
       {-             , optBrkStyle = brkStyle-}
       {-             , optGrpStyle = grpStyle } = opts-}
       {- when verbose (hPutStrLn stderr "More chatty!")-}
       return (opts, refStr, files)
    (_, _, errs) -> do
       prg <- getProgName
       hPutStrLn stderr $ "\n" ++ prg ++ ": Could not parse commandline options, because:"
       hPutStrLn stderr $ concat $ "  * " : intersperse "  * " errs
       printUsageInfo
       exitWith $ ExitFailure 200

printUsageInfo = do
  prg <- getProgName
  hPutStrLn stderr $ usageInfo (
    "Usage: " ++ prg ++ " [OPTION...] REF_STRING QTF_FILE...\n" ++
    "  REF_STRING:  Comma separated Quran references that may contain verse ranges.\n" ++
    "  QTF_FILE:    QTF-formated file. (multiple QTF_FILEs allowed)\n" ++
    "  OPTIONs:"
    ) options
  hPutStrLn stderr $ "For example:\n" ++
                     "  " ++ prg ++ " -v 1:1-7,2:256,31:6 en.assad.txt en.pickthall.txt"


-- INTERNAL

defaultOptions = Options
  { optVerbose  = False
  , optQpfFile  = "./default.qpf"
  , optBrkStyle = True
  , optGrpStyle = AllBreaks
  }

options :: [OptDescr (Options -> IO Options)]
options =
  [ Option ['v'] ["verbose"] (NoArg (\o -> return o { optVerbose = True }))
                             "Makes running more verbose."
  , Option ['p'] ["qpf"]     (ReqArg (\a o -> return o { optQpfFile = a }) "QPF_FILE")
                             "Paragraphing in the QPF format."
  , Option ['n'] ["no-brk"]  (NoArg (\o -> return o { optBrkStyle = False }))
                             "Group verses on reference ranges from REF_STRING only."
  , Option ['R'] ["grp-rng"] (NoArg (\o -> return o { optGrpStyle = RefRangesOnly }))
                             "Group verses on reference ranges from REF_STRING only."
  , Option ['B'] ["grp-big"] (NoArg (\o -> return o { optGrpStyle = BigBreaks }))
                             "Group verses on big breaks (and renges)."
  , Option ['A'] ["grp-all"] (NoArg (\o -> return o { optGrpStyle = AllBreaks }))
                             "Group verses on all breaks (and ranges)."
  , Option ['I'] ["grp-ref"] (NoArg (\o -> return o { optGrpStyle = ByVerse }))
                             "Group verses individually."
  , Option ['V'] ["version"] (NoArg (\_ -> do hPutStrLn stdout $ "Version 0.0.3"
                                              exitWith ExitSuccess))
                             "Only show version number and exit."
  , Option ['h'] ["help"]    (NoArg (\_ -> do printUsageInfo
                                              exitWith ExitSuccess))
                             "Show (this) usage information."
  ]


