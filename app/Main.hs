{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Lib
import           Data.List (transpose)
import           Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           System.Console.GetOpt (OptDescr(..), ArgDescr(..), ArgOrder(..), usageInfo, getOpt)
import           System.Directory (createDirectoryIfMissing)
import           System.Environment (getArgs, getProgName)
import           System.Exit (exitSuccess)
import           System.IO (hPutStrLn, stderr)
import           System.FilePath (takeDirectory)

data Options = Options
  { optInputAgda         :: Either FilePath (IO T.Text)
  , optInputHTML         :: Maybe (IO T.Text)
  , optOutputFile        :: T.Text -> IO ()
  , optVerbose           :: Bool
  , optStripImplicitArgs :: T.Text -> T.Text
  , optLinkToAgdaStdlib  :: T.Text -> T.Text
  , optUseJekyll         :: Maybe FilePath
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
  , Option "v" ["verbose"]
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
  , Option [] ["help"]
    (NoArg  (\_ -> do
               prg <- getProgName
               hPutStrLn stderr (usageInfo prg options)
               exitSuccess))
    "Show help"
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
              } = opts

  let (istreamAgda', maybeInputFile) = maybeReadInput istreamAgda
  agdaSource <- istreamAgda'
  htmlSource <- fromMaybe (Lib.callAgdaToHTML verbose useJekyll maybeInputFile agdaSource) istreamHTML

  let
    textBlocks = Lib.text agdaSource
    codeBlocks = map (linkToAgdaStdlib . stripImplicitArgs) (Lib.code (isJust useJekyll) htmlSource)

  ostream . T.concat $ blend [textBlocks, codeBlocks]

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
