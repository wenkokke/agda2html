{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Control.Monad (forM_,when)
import qualified Data.Foldable as F
import Data.Maybe (fromMaybe)
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.ICU (Regex,MatchOption(..),regex,find,group)
import Data.Text.ICU.Replace (replaceAll)
import System.Directory (copyFile)
import System.Directory.Tree (AnchoredDirTree(..),readDirectoryWith)
import System.IO (stderr)
import System.IO.Temp (withSystemTempDirectory)
import System.Exit (ExitCode(..),exitWith)
import System.FilePath
import System.Process (StdStream(..),CreateProcess(..),createProcess_,callProcess,proc,waitForProcess)


findModuleName :: T.Text -> T.Text
findModuleName src = fromMaybe "Lib" (group 1 =<< find reModuleName src)
  where
    reModuleName :: Regex
    reModuleName = regex [Multiline] "^module\\s+(\\S+)\\s+where"


callAgdaToHTML :: Bool -> Maybe FilePath -> T.Text -> IO T.Text
callAgdaToHTML v inputFile src = do
  withSystemTempDirectory "agda2html" $ \tempDir -> do

    -- Prepare calling agda --html:
    (inputFile',outputFile) <-
      case inputFile of
        { Nothing -> do
            let moduleName = T.unpack (findModuleName src)
                inputFile' = tempDir </> moduleName <.> "lagda"
                outputFile = tempDir </> moduleName <.> "html"
            T.writeFile inputFile' src
            return (inputFile',outputFile)
        ; Just inputFile' -> do
            let (dir,fn)    = splitFileName inputFile'
                inputFile'' = tempDir </> fn
                outputFile  = tempDir </> "html" </> fn -<.> "html"
            agdaFiles <- agdaFilesIn dir
            forM_ agdaFiles $ \source -> do
              let target = tempDir </> takeFileName source
              copyFile source target
            return (inputFile'',outputFile)
        }

    -- Call agda --html:
    (_, Just hout, Just herr, pid) <-
      createProcess_ "agda"
      ((proc "agda" ["--allow-unsolved-metas"
                    ,"--html"
                    ,"--html-dir=html"
                    ,inputFile'])
        { cwd     = Just tempDir
        , std_in  = NoStream
        , std_out = CreatePipe
        , std_err = CreatePipe })

    -- If agda does not fail:
    exitCode <- waitForProcess pid
    when v $ do
      out <- T.hGetContents hout
      err <- T.hGetContents herr
      T.hPutStrLn stderr (T.append out err)
    case exitCode of
      ExitSuccess   -> T.readFile outputFile
      ExitFailure e -> do
        out <- T.hGetContents hout
        err <- T.hGetContents herr
        T.hPutStrLn stderr (T.append out err)
        exitWith (ExitFailure e)


text :: T.Text -> [T.Text]
text = enter
  where
    enter :: T.Text -> [T.Text]
    enter t0 | T.null t0 = []
             | otherwise =
               let (t1,x0) = T.breakOn "\\begin{code}" t0
               in  t1 : leave x0

    leave :: T.Text -> [T.Text]
    leave x0 | T.null x0 = []
             | otherwise =
               let (x1,t0) = T.breakOn "\\end{code}" x0
                   (_ ,t1) = T.breakOn "\n"          t0
               in  enter (T.drop 1 t1)


code :: T.Text -> [T.Text]
code = enter
  where
    enter :: T.Text -> [T.Text]
    enter t0 | T.null t0 = []
             | otherwise =
               let (_ ,t1) = T.breakOn    "\\begin{code}" t0
                   (w0,x0) = T.breakOn    "</a"           t1
                   (_ ,w1) = T.breakOnEnd "\n"            w0
                   (_ ,x1) = T.breakOn    ">"             x0
               in  leave (T.append w1 (T.drop 1 x1))

    leave :: T.Text -> [T.Text]
    leave t0 | T.null t0 = []
             | otherwise =
               let (x0,t1) = T.breakOn    "\\end{code}" t0
                   (x1,_ ) = T.breakOnEnd "<a"          x0
                   (_ ,t2) = T.breakOn    "</a"         t1
                   (_ ,t3) = T.breakOn    ">"           t2
                   blCode  = T.dropEnd 2 x1
                   blCode' =
                     if T.null (T.strip blCode)
                     then T.empty
                     else T.unlines ["<pre class=\"Agda\">{% raw %}"
                                    ,blCode
                                    ,"{% endraw %}</pre>"]
               in  blCode' : enter (T.drop 1 t3)


-- |Correct references to the Agda stdlib.
correctStdLibHref :: IO (T.Text -> T.Text)
correctStdLibHref =
  reStdlibHref >>= \re ->
  return (replaceAll re "https://agda.github.io/agda-stdlib/$1.html")


-- |Remove implicit arguments from Agda HTML.
removeImplicit :: T.Text -> T.Text
removeImplicit = replaceAll reImplicit ""


-- |An ICU regular expression which matches implicit parameters in Agda HTMl.
reImplicit :: Regex
reImplicit = regex [DotAll] $ T.concat
  ["((<a[^>]*>\\s*∀\\s*<\\/a[^>]*>)(<a[^>]*>\\s*<\\/a[^>]*>)*)?"
  ,"<a[^>]*>\\s*\\{\\s*<\\/a[^>]*>"
  ,"(<a[^>]*>[^=\\}]*<\\/a[^>]*>)*"
  ,"<a[^>]*>\\s*\\}\\s*<\\/a[^>]*>"
  ,"((<a[^>]*>\\s*<\\/a[^>]*>)*(<a[^>]*>\\s*→\\s*<\\/a[^>]*>))?"
  ,"(<a[^>]*>\\s*<\\/a[^>]*>)*"]




-- |An ICU regular expression which matches links to the Agda stdlib.
reStdlibHref :: IO Regex
reStdlibHref = do
  modNames <- map T.pack <$> stdlibModules
  let
    modPatns = T.replace "." "\\." <$> modNames
    modPatn  = T.concat . L.intersperse "|" $ modPatns
    refPatn  = "(" `T.append` modPatn `T.append` ")\\.html"
  return (regex [] refPatn)


-- |A url pointing to the GitHub repository of the Agda stdlib.
stdlibUrl :: String
stdlibUrl = "git@github.com:agda/agda-stdlib.git"


-- |Generate a list of the Agda stdlib modules.
stdlibModules :: IO [String]
stdlibModules =
  withSystemTempDirectory "agda-stdlib" $ \tempDir -> do

    -- call `git clone git@github.com:agda/agda-stdlib.git tempDir`
    callProcess "git" ["clone","--depth=1","-q",stdlibUrl,tempDir]

    -- collect all files in the repository
    agdaFiles <- agdaFilesIn tempDir

    let
      sep2dot :: Char -> Char
      sep2dot c | c == pathSeparator = '.'
                | otherwise          = c

      file2mod :: FilePath -> String
      file2mod = map sep2dot . dropExtension . makeRelative (tempDir </> "src")

    return $ map file2mod agdaFiles


-- |Gather all .agda and .lagda files within a directory.
agdaFilesIn :: FilePath -> IO [FilePath]
agdaFilesIn dir = do
  _ :/ tree <- readDirectoryWith return dir
  let
    isAgdaSrc :: FilePath -> Bool
    isAgdaSrc = (`elem`[".agda",".lagda"]) . takeExtension
  return $ filter isAgdaSrc (F.toList tree)
