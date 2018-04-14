{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Control.Monad (forM)
import qualified Data.Foldable as F
import Data.Function ((&))
import Data.String (fromString)
import Data.Maybe (fromMaybe,fromJust,isJust)
import Data.Monoid ((<>))
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.ICU (Regex,MatchOption(..),regex,find,group)
import Data.Text.ICU.Replace (replaceAll,rstring)
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


callAgdaToHTML :: Bool -> Maybe FilePath -> Maybe FilePath -> T.Text -> IO T.Text
callAgdaToHTML v jekyllRoot inputFile src = do
  withSystemTempDirectory "agda2html" $ \tempDir -> do

    -- Prepare calling agda --html:
    (inputFile',outputFile,agdaFiles) <-
      case inputFile of
        { Nothing -> do
            let moduleName = T.unpack (findModuleName src)
                inputFile' = tempDir </> moduleName <.> "lagda"
                outputFile = tempDir </> moduleName <.> "html"
            T.writeFile inputFile' src
            return (inputFile',outputFile,[])
        ; Just inputFile' -> do
            let (dir,fn)    = splitFileName inputFile'
                inputFile'' = tempDir </> fn
                outputFile  = tempDir </> "html" </> fn -<.> "html"
            agdaFiles <- agdaFilesIn dir
            agdaFiles' <- forM agdaFiles $ \source -> do
              let target = tempDir </> takeFileName source
              copyFile source target
              return target
            return (inputFile'',outputFile,agdaFiles')
        }

    -- Call agda --html:
    let std_opt = if v then UseHandle stderr else CreatePipe

    (_, hout, herr, pid) <-
      createProcess_ "agda"
      ((proc "agda" ["--allow-unsolved-metas"
                    ,"--html"
                    ,"--html-dir=html"
                    ,inputFile'])
        { cwd     = Just tempDir
        , std_in  = NoStream
        , std_out = std_opt
        , std_err = std_opt })

    -- If agda does not fail:
    exitCode <- waitForProcess pid
    case exitCode of
      ExitSuccess   -> do
        srcHTML <- T.readFile outputFile
        if isJust jekyllRoot
          then return $ liquidifyLocalHref (fromJust jekyllRoot) agdaFiles srcHTML
          else return $ srcHTML
      ExitFailure e -> do
        let (Just hout') = hout
            (Just herr') = herr
        out <- T.hGetContents hout'
        err <- T.hGetContents herr'
        T.hPutStrLn stderr (T.append out err)
        exitWith (ExitFailure e)

preOpen, preClose, codeOpen, codeClose :: T.Text
preOpen   = "<pre class=\"Agda\">"
preClose  = "</pre>"
codeOpen  = "\\begin{code}"
codeClose = "\\end{code}"

text :: T.Text -> [T.Text]
text = enter
  where
    enter :: T.Text -> [T.Text]
    enter t0 | T.null t0 = []
             | otherwise =
               let (t1,c1) = T.breakOn codeOpen t0
                   c2 = T.drop (T.length codeOpen) c1
               in  t1 : leave c2

    leave :: T.Text -> [T.Text]
    leave c0 | T.null c0 = []
             | otherwise = c0
                         & T.breakOn codeClose
                         & snd
                         & T.drop (T.length codeClose)
                         & enter


code :: Bool -> T.Text -> [T.Text]
code jekyll = enter
  where
    rawOpen
      | jekyll    = "{% raw %}"
      | otherwise = ""
    rawClose
      | jekyll    = "{% endraw %}"
      | otherwise = ""

    enter :: T.Text -> [T.Text]
    enter t0 | T.null t0 = []
             | otherwise = t0
                         & T.breakOn codeOpen
                         & snd
                         & T.drop (T.length codeOpen)
                         & leave

    leave :: T.Text -> [T.Text]
    leave t0 | T.null t0 = []
             | otherwise = c3 : enter t2
               where
                 (c1,t1) = T.breakOn codeClose t0
                 t2 = T.drop (T.length codeClose) t1
                 c2 = T.strip c1
                 c3 = if T.null c2
                      then T.empty
                      else T.concat [preOpen,rawOpen,c2,rawClose,preClose]

-- |Correct references to local files.
liquidifyLocalHref :: FilePath -> [FilePath] -> T.Text -> T.Text
liquidifyLocalHref jekyllRoot paths =
  replaceAll (reLocal paths) . fromString $
    "\"{% endraw %}{{ site.baseurl }}{% link " <> jekyllRoot <> "$1.md %}{% raw %}$2\""


-- |An ICU regular expression which matches links to local files.
reLocal :: [FilePath] -> Regex
reLocal paths = regex [] refrPatn
  where
    filePatn = T.concat . L.intersperse "|" . map (T.pack . takeBaseName) $ paths
    refrPatn = "[\"'](" `T.append` filePatn `T.append` ")\\.html(#[^\"^']+)?[\"']"


-- |Correct references to the Agda stdlib.
correctStdLibHref :: IO (T.Text -> T.Text)
correctStdLibHref =
  reStdlibHref >>= \re ->
  return (replaceAll re "\"https://agda.github.io/agda-stdlib/$1.html\"")


-- |Remove implicit arguments from Agda HTML.
removeImplicit :: T.Text -> T.Text
removeImplicit = replaceAll reImplicit ""


-- |An ICU regular expression which matches implicit parameters in Agda HTMl.
reImplicit :: Regex
reImplicit = regex [DotAll] $ T.concat
  ["((<a[^>]*>\\s*",reForall,"\\s*<\\/a[^>]*>)\\s*(<a[^>]*>\\s*<\\/a[^>]*>)*\\s*)?"
  ,"<a[^>]*>\\s*\\{\\s*<\\/a[^>]*>\\s*"
  ,"(<a[^>]*>[^=\\}]*<\\/a[^>]*>)*\\s*"
  ,"<a[^>]*>\\s*\\}\\s*<\\/a[^>]*>\\s*"
  ,"((<a[^>]*>\\s*<\\/a[^>]*>)*\\s*(<a[^>]*>\\s*",reRightArrow,"\\s*<\\/a[^>]*>)\\s*)?"
  ,"(<a[^>]*>\\s*<\\/a[^>]*>)*\\s*"]
  where
    reForall = "(∀|&#8704;|&#x2200;|&forall;)"
    reRightArrow = "(→|&#8594;|&#x2192;|&rarr;)"



-- |An ICU regular expression which matches links to the Agda stdlib.
reStdlibHref :: IO Regex
reStdlibHref = do
  modNames <- map T.pack <$> stdlibModules
  let
    builtin   = "Agda(\\.[A-Za-z]+)*"
    modPatns  = T.replace "." "\\." <$> modNames
    modPatn   = T.concat . L.intersperse "|" $ builtin : modPatns
    refPatn   = "[\"'](" `T.append` modPatn `T.append` ")\\.html(#[^\"^']+)?[\"']"
  return (regex [] refPatn)


-- |A url pointing to the GitHub repository of the Agda stdlib.
stdlibUrl :: String
stdlibUrl = "https://github.com/agda/agda-stdlib.git"


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
