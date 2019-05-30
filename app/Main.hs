{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Lib
import           Data.List (transpose)
import           Data.Maybe (fromMaybe, isJust)
import           Data.String (fromString)
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
    "Input Agda file (optional)."
  , Option [] ["input-html"]
    (ReqArg (\arg opt -> return opt { optInputHTML = Just (T.readFile arg) })
            "FILE")
    "Input HTML file (optional)."
  , Option "o" ["output"]
    (ReqArg (\arg opt -> return opt { optOutputFile = writeFileCreateDirectoryIfMissing arg })
            "FILE")
    "Output file (optional)."
  , Option [] ["verbose"]
    (NoArg (\opt -> return opt { optVerbose = True }))
    "Verbose output."
  , Option [] ["strip-implicit-args"]
    (NoArg (\opt -> return opt { optStripImplicitArgs = Lib.removeImplicit }))
    "Strip implicit arguments."
  , Option [] ["link-to-agda-stdlib"]
    (OptArg (\arg opt -> do
               f <- Lib.correctStdLibHref (fromMaybe Lib.defaultStdLibHref (fromString <$> arg))
               return opt { optLinkToAgdaStdlib = f })
            "AGDA_STDLIB_BASEURL")
    "Fix links to the Agda stdlib."
  , Option "" ["link-to-local-agda-names"]
    (NoArg (\opt -> return opt { optLocalRefSugar = True }))
    "Add syntax for linking to Agda names. Only works when using Jekyll."
  , Option [] ["use-jekyll"]
    (ReqArg (\arg opt -> return opt { optUseJekyll = Just arg })
            "JEKYLL_ROOT")
    "Fix links to Jekyll posts and wrap code in {% raw %} tags."
  , Option "h" ["help"]
    (NoArg  (\_ -> do
               prg <- getProgName
               hPutStrLn stderr (usageInfo prg options)
               exitSuccess))
    "Show help."
  , Option "v" ["version"]
    (NoArg (\_ -> do
               hPutStrLn stdout $ "agda2html " ++ showVersion version
               exitSuccess))
    "Show version."
  ]

main :: IO ()
main = do
  (actions, userArgs, _) <- getOpt Permute options <$> getArgs

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

  fileContext <- Lib.getFileContext maybeInputFile agdaSource
  
  htmlSource <- fromMaybe (Lib.callAgdaToHTML verbose userArgs useJekyll maybeInputFile agdaSource fileContext) istreamHTML

  let
    rawTextBlocks = Lib.text agdaSource
    codeBlocks = map (linkToAgdaStdlib . stripImplicitArgs) (Lib.code (isJust useJekyll) htmlSource)


  
  textBlocks <- (if localRefSugar then mapM (Lib.postProcess useJekyll maybeInputFile fileContext) rawTextBlocks
                                  else return rawTextBlocks)

  ostream $ T.concat $ blend [textBlocks, codeBlocks]

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
