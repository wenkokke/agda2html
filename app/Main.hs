{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Lib
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           System.Console.GetOpt (OptDescr(..),ArgDescr(..),ArgOrder(..),usageInfo,getOpt)
import           System.Environment (getArgs,getProgName)
import           System.Exit (exitSuccess)
import           System.IO (hPutStrLn,stderr)



data Options = Options
  { optInputAgda         :: Either FilePath (IO T.Text)
  , optInputHTML         :: Maybe (IO T.Text)
  , optOutputFile        :: T.Text -> IO ()
  , optVerbose           :: Bool
  , optStripImplicitArgs :: T.Text -> T.Text
  , optLinkToAgdaStdlib  :: T.Text -> T.Text
  , optJekyll            :: Bool
  }

defaultOptions :: Options
defaultOptions = Options
  { optInputAgda         = Right T.getContents
  , optInputHTML         = Nothing
  , optOutputFile        = T.putStrLn
  , optVerbose           = False
  , optStripImplicitArgs = id
  , optLinkToAgdaStdlib  = id
  , optJekyll            = False
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
    (ReqArg (\arg opt -> do
                return opt { optOutputFile = T.writeFile arg })
            "FILE")
    "Output file (optional)"
  , Option "v" ["verbose"]
    (NoArg (\opt -> return opt { optVerbose = True }))
    "Verbose output"
  , Option [] ["strip-implicit-args"]
    (NoArg (\opt -> do
               return opt { optStripImplicitArgs = Lib.removeImplicit }))
    "Strip implicit arguments"
  , Option [] ["link-to-agda-stdlib"]
    (NoArg (\opt -> do
               f <- Lib.correctStdLibHref
               return opt { optLinkToAgdaStdlib = f }))
    "Fix links to the Agda stdlib"
  , Option [] ["jekyll"]
    (NoArg (\opt -> return opt { optJekyll = True }))
    "Fix links to Jekyll posts and wrap code in {% raw %} tags."
  , Option [] ["help"]
    (NoArg  (\_ -> do
               prg <- getProgName
               hPutStrLn stderr (usageInfo prg options)
               exitSuccess))
    "Show help"
  ]


readInput :: Either FilePath (IO T.Text) -> (IO T.Text, Maybe FilePath)
readInput (Right t) = (t, Nothing)
readInput (Left fn) = (T.readFile fn, Just fn)


main :: IO ()
main = do
  (actions, _, _) <- getOpt Permute options <$> getArgs

  opts <- foldl (>>=) (return defaultOptions) actions
  let Options { optInputAgda         = istreamAgda'
              , optInputHTML         = istreamHTML
              , optOutputFile        = ostream
              , optVerbose           = v
              , optStripImplicitArgs = stripImplicitArgs
              , optLinkToAgdaStdlib  = linkToAgdaStdlib
              , optJekyll            = j
              } = opts

  let (istreamAgda,fn) = readInput istreamAgda'
  srcAgda <- istreamAgda
  srcHTML <- fromMaybe (Lib.callAgdaToHTML v j fn srcAgda) istreamHTML

  let
    blText = Lib.text srcAgda
    blCode = map (linkToAgdaStdlib . stripImplicitArgs) (Lib.code j srcHTML)

    merge    []  ys = ys
    merge (x:xs) ys = x : merge ys xs

  ostream (T.concat (merge blText blCode))
