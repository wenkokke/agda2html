{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Lib
import           Data.List (transpose)
import           Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Version (showVersion)
import           Paths_agda2html (version)
import           System.Console.GetOpt (OptDescr(..), ArgDescr(..), ArgOrder(..), usageInfo, getOpt)
import           System.Directory (createDirectoryIfMissing)
import           System.Environment (getArgs, getProgName)
import           System.Exit (exitSuccess)
import           System.IO (hPutStrLn, stderr, stdout)
import           System.FilePath (takeDirectory)

data Options = Options
  { optInputAgda         :: Either FilePath (IO T.Text)
  , optInputHTML         :: Maybe (IO T.Text)
  , optOutputFile        :: T.Text -> IO ()
  , optVerbose           :: Bool
  , optStripImplicitArgs :: T.Text -> T.Text
  , optLinkToAgdaStdlib  :: T.Text -> T.Text
  , optUseJekyll         :: Maybe FilePath
  , optLocalRefSugar     :: Bool
  }

defaultOptions :: Options
defaultOptions = Options
  { optInputAgda         = Right T.getContents
  , optInputHTML         = Nothing
  , optOutputFile        = T.putStrLn
  , optVerbose           = False
  , optStripImplicitArgs = id
  , optLinkToAgdaStdlib  = id
  , optUseJekyll         = Nothing
  , optLocalRefSugar     = False
  }

options :: [ OptDescr (Options -> IO Options) ]
options =
  [ Option "i" ["input"]
    (ReqArg (\arg opt -> return opt { optInputAgda = Left arg })
            "FILE")
    "Input Agda file (optional)"
  , Option [] ["input-html"]
    (ReqArg (\arg opt -> return opt { optInputHTML = Just (T.readFile arg) })
            "FILE")
    "Input HTML file (optional)"
  , Option "o" ["output"]
    (ReqArg (\arg opt -> return opt { optOutputFile = writeFileCreateDirectoryIfMissing arg })
            "FILE")
    "Output file (optional)"
  , Option [] ["verbose"]
    (NoArg (\opt -> return opt { optVerbose = True }))
    "Verbose output"
  , Option [] ["strip-implicit-args"]
    (NoArg (\opt -> return opt { optStripImplicitArgs = Lib.removeImplicit }))
    "Strip implicit arguments"
  , Option [] ["link-to-agda-stdlib"]
    (NoArg (\opt -> do
               f <- Lib.correctStdLibHref
               return opt { optLinkToAgdaStdlib = f }))
    "Fix links to the Agda stdlib"
  , Option [] ["use-jekyll"]
    (ReqArg (\arg opt -> return opt { optUseJekyll = Just arg })
            "JEKYLL_ROOT")
    "Fix links to Jekyll posts and wrap code in {% raw %} tags."
  , Option "h" ["help"]
    (NoArg  (\_ -> do
               prg <- getProgName
               hPutStrLn stderr (usageInfo prg options)
               exitSuccess))
    "Show help"
  , Option "v" ["version"]
    (NoArg (\_ -> do
               hPutStrLn stdout $ "agda2html " ++ showVersion version
               exitSuccess))
    "Show version"
  , Option "lrs" ["local-references"]
    (NoArg (\opt -> return opt { optLocalRefSugar = True }))
    "Enable cleaner syntax for local references. Only works when using Jekyll"
  ]

main :: IO ()
main = do
  (actions, _, _) <- getOpt Permute options <$> getArgs

  opts <- foldl (>>=) (return defaultOptions) actions
  let Options { optInputAgda         = istreamAgda
              , optInputHTML         = istreamHTML
              , optOutputFile        = ostream
              , optVerbose           = verbose
              , optStripImplicitArgs = stripImplicitArgs
              , optLinkToAgdaStdlib  = linkToAgdaStdlib
              , optUseJekyll         = useJekyll
              , optLocalRefSugar     = localRefSugar
              } = opts

  let (istreamAgda', maybeInputFile) = maybeReadInput istreamAgda
  agdaSource <- istreamAgda'
  --rawAgdaSource <- istreamAgda'
  --agdaSource <- if optLocalRefSugar opts then Lib.preProcess useJekyll maybeInputFile rawAgdaSource
  --                                          else return rawAgdaSource
  htmlSource <- fromMaybe (Lib.callAgdaToHTML verbose useJekyll maybeInputFile agdaSource) istreamHTML

  let
    textBlocks = Lib.text agdaSource
    codeBlocks = map (linkToAgdaStdlib . stripImplicitArgs) (Lib.code (isJust useJekyll) htmlSource)
    allBlocks  = T.concat $ blend [textBlocks, codeBlocks]

  output <- (if optLocalRefSugar opts then Lib.postProcess useJekyll maybeInputFile allBlocks
                                      else return allBlocks)

  ostream output

-- |Writes to a file, creating the directories if missing.
writeFileCreateDirectoryIfMissing :: FilePath -> T.Text -> IO ()
writeFileCreateDirectoryIfMissing file text = do
  createDirectoryIfMissing True (takeDirectory file)
  T.writeFile file text

-- |Reads a file in `Left`, or passes on the file contents in `Right`
maybeReadInput :: Either FilePath (IO T.Text) -> (IO T.Text, Maybe FilePath)
maybeReadInput (Right t) = (t, Nothing)
maybeReadInput (Left fn) = (T.readFile fn, Just fn)

-- |Blends a list of list, taking elements from each list in turn.
blend :: [[a]] -> [a]
blend = concat . transpose
