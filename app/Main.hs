{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Lib
import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import           Data.List (isSuffixOf,elemIndex)
import           Data.Maybe (fromMaybe,isJust)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Network.HTTP.Conduit (simpleHttp)
import           System.Console.GetOpt (OptDescr(..),ArgDescr(..),
                                        ArgOrder(..),usageInfo,getOpt)
import           System.Directory (doesFileExist,doesDirectoryExist,getHomeDirectory)
import           System.Environment (getArgs,getProgName)
import           System.Exit (exitSuccess)
import           System.FilePath ((</>),(<.>),takeBaseName)
import           System.FilePath.Glob (globDir1,compile)
import           System.IO (hPutStrLn,stderr)

data Options = Options
  { optInputAgda                :: Either FilePath (IO T.Text)
  , optInputHTML                :: Maybe (IO T.Text)
  , optOutputFile               :: T.Text -> IO ()
  , optVerbose                  :: Bool
  , optLinkToLibrary            :: [(Library, Maybe UrlTemplate)]
  , optStripImplicitArgs        :: T.Text -> T.Text
  , optLinkToAgdaStdlib         :: T.Text -> T.Text
  , optJekyllRoot               :: Maybe FilePath
  , optAgdaVersionInFrontMatter :: Bool
  }

defaultOptions :: Options
defaultOptions = Options
  { optInputAgda                = Right T.getContents
  , optInputHTML                = Nothing
  , optOutputFile               = T.putStrLn
  , optVerbose                  = False
  , optLinkToLibrary            = []
  , optStripImplicitArgs        = id
  , optLinkToAgdaStdlib         = id
  , optJekyllRoot               = Nothing
  , optAgdaVersionInFrontMatter = False
  }

type UrlTemplate = T.Text

data Library
  = LibraryPath { libLibraryPath          :: FilePath }
  | GitHub      { libGitHubAuthor         :: T.Text
                , libGitHubRepositoryName :: T.Text }

data GitHubFile = GitHubFile
  { ghName        :: String
  , ghPath        :: FilePath
  , ghSHA         :: String
  , ghSize        :: Int
  , ghUrl         :: String
  , ghHtmlUrl     :: String
  , ghGitUrl      :: String
  , ghDownloadUrl :: String
  , ghType        :: String
  }

instance FromJSON GitHubFile where
  parseJSON = withObject "githubfile" $ \o -> do
    ghName        <- o .: "name"
    ghPath        <- o .: "path"
    ghSHA         <- o .: "sha"
    ghSize        <- o .: "size"
    ghUrl         <- o .: "url"
    ghHtmlUrl     <- o .: "html_url"
    ghGitUrl      <- o .: "git_url"
    ghDownloadUrl <- o .: "download_url"
    ghType        <- o .: "type"
    return GitHubFile{..}

-- |Check if the given path ends with .agda-lib and points to a file.
isAgdaLib :: FilePath -> IO Bool
isAgdaLib path
  | ".agda-lib" `isSuffixOf` path = doesFileExist path
  | otherwise                     = return False

-- |Check if the library is a well-known library with public clickable HTML.
inferUrlTemplate :: Library -> Maybe UrlTemplate
inferUrlTemplate (LibraryPath path)
  | takeBaseName path == "standard-library"
  = Just "https://agda.github.io/agda-stdlib/{module}.html{anchor}"
inferUrlTemplate (GitHub author repo)
  | author == "agda" && repo == "agda-stdlib"
  = Just "https://agda.github.io/agda-stdlib/{module}.html{anchor}"
inferUrlTemplate _ = Nothing

-- |Resolve the library name or repository and check it is locally available.
inferLibrary :: String -> IO (Library, Maybe UrlTemplate)
inferLibrary lib =
  libIsLocalLibrary $ libIsPathToAgdaLib $ libIsDirWithAgdaLib $ libIsGitHubRepo $ libPanic
  where
    -- Check if `lib` is the name of a locally available library.
    libIsLocalLibrary :: (IO (Library, Maybe UrlTemplate) -> IO (Library, Maybe UrlTemplate)) -> IO (Library, Maybe UrlTemplate)
    libIsLocalLibrary k
      | '/' `elem` lib = k _
      | otherwise      =
        do home <- getHomeDirectory
           let path = home </> ".agda" </> lib <.> "agda-lib"
           libIsAgdaLib <- isAgdaLib $ path
           if libIsAgdaLib
             then do let lcl = LibraryPath path
                     let url = inferUrlTemplate lcl
                     return (lcl, url)
             else k _

    -- Check if `lib` is a path to an *.agda-lib file.
    libIsPathToAgdaLib :: (IO (Library, Maybe UrlTemplate) -> IO (Library, Maybe UrlTemplate)) -> IO (Library, Maybe UrlTemplate) -> IO (Librar
    libIsPathToAgdaLib cont = do
      libIsAgdaLib <- isAgdaLib lib
      if libIsAgdaLib
        then do let lcl = LibraryPath lib
                let url = inferUrlTemplate lcl
                return (lcl, url)
        else cont

    -- Check if `lib` is a path to a directory with an *.agda-lib file in there.
    libIsDirWithAgdaLib :: IO (Library, Maybe UrlTemplate) -> IO (Library, Maybe UrlTemplate)
    libIsDirWithAgdaLib cont = do
      libIsDir <- doesDirectoryExist lib
      if libIsDir
        then do let patn = compile $ lib </> "*.agda-lib"
                libAgdaLibFiles <- globDir1 patn lib
                case libAgdaLibFiles of
                  []               -> cont
                  [libAgdaLibFile] -> return (LibraryPath libAgdaLibFile, Nothing)
                  _                -> error $
                    "I refuse to choose between the .agda-lib files in " ++ lib
        else cont

    -- Check if `lib` is a valid GitHub repository.
    libIsGitHubRepo :: IO (Library, Maybe UrlTemplate) -> IO (Library, Maybe UrlTemplate)
    libIsGitHubRepo cont =
      -- If it's not of the form "author/repo", then it's definitely not a GitHub repository.
      case '/' `elemIndex` lib of
        Nothing -> cont
        Just ix -> do let (githubAuthor, githubRepo) = splitAt ix lib
                      _

    -- Panic and shout light abuse at the user.
    libPanic :: IO (Library, Maybe UrlTemplate)
    libPanic = error $
      "I'm sorry, I don't know which library you mean by '" ++ lib ++ "'"

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
    (ReqArg (\arg opt -> return opt { optOutputFile = T.writeFile arg })
            "FILE")
    "Output file (optional)"
  , Option "v" ["verbose"]
    (NoArg (\opt -> return opt { optVerbose = True }))
    "Verbose output"
  , Option [] ["strip-implicit-args"]
    (NoArg (\opt -> return opt { optStripImplicitArgs = Lib.removeImplicit }))
    "Strip implicit arguments"
  , Option [] ["link-to"]
    (ReqArg (\arg opt -> do
                lib <- inferLibrary arg
                return opt { optLinkToLibrary = lib : optLinkToLibrary opt })
      "LIBRARY")
    "Fix link to an Agda library"
  , Option [] ["link-to-agda-stdlib"]
    (NoArg (\opt -> do
               f <- Lib.correctStdLibHref
               return opt { optLinkToAgdaStdlib = f }))
    "Fix links to the Agda stdlib"
  , Option [] ["jekyll-root"]
    (ReqArg (\arg opt -> return opt { optJekyllRoot = Just arg })
            "FILE")
    "Fix links to Jekyll posts and wrap code in {% raw %} tags."
  , Option [] ["agda-version-in-front-matter"]
    (NoArg (\opt -> return opt { optAgdaVersionInFrontMatter = True }))
    "Add the Agda version to the front matter of the output file"
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
  let Options { optInputAgda                = istreamAgda'
              , optInputHTML                = istreamHTML
              , optOutputFile               = ostream
              , optVerbose                  = v
              , optLinkToLibrary            = libs
              , optStripImplicitArgs        = stripImplicitArgs
              , optLinkToAgdaStdlib         = linkToAgdaStdlib
              , optJekyllRoot               = j
              , optAgdaVersionInFrontMatter = agdaVersionInFrontMatter
              } = opts

  let (istreamAgda,fn) = readInput istreamAgda'
  srcAgda <- istreamAgda
  srcHTML <- fromMaybe (Lib.callAgdaToHTML v j fn srcAgda) istreamHTML

  let
    blText = Lib.text srcAgda
    blCode = map (linkToAgdaStdlib . stripImplicitArgs) (Lib.code (isJust j) srcHTML)

    merge    []  ys = ys
    merge (x:xs) ys = x : merge ys xs

  ostream (T.concat (merge blText blCode))
