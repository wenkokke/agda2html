{-# LANGUAGE OverloadedStrings #-}
module Lib where

import qualified Data.Foldable as F
import           Data.Function ((&))
import           Data.String (fromString)
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Text.ICU (Regex, MatchOption(..), regex, find, group)
import           Data.Text.ICU.Replace (replaceAll)
import           System.Directory (createDirectoryIfMissing, getCurrentDirectory, canonicalizePath)
import           System.Directory.Tree (AnchoredDirTree(..), readDirectoryWith)
import           System.IO (stderr)
import           System.IO.Temp (withSystemTempDirectory)
import           System.Exit (ExitCode(..), exitWith)
import           System.FilePath
import           System.Process (StdStream(..), CreateProcess(..), createProcess_, callProcess, proc, waitForProcess)

-- |Extracts the module name from an Agda file.
getModuleName :: T.Text -> T.Text
getModuleName src = fromMaybe "Main" (group 1 =<< find reModuleName src)
  where
    reModuleName :: Regex
    reModuleName = regex [Multiline] "^module\\s+(\\S+)\\s+where"

makeAbsolute :: FilePath -> IO FilePath
makeAbsolute path
  | isAbsolute path = return path
  | otherwise       = do
      currentDirectory <- getCurrentDirectory
      canonicalizePath (currentDirectory </> path)
  
-- |Creates a temporary directory, calls `agda --html`, and generates the HTML
--  output in the temporary directory.
callAgdaToHTML :: Bool -> Maybe FilePath -> Maybe FilePath -> T.Text -> IO T.Text
callAgdaToHTML verbose useJekyll maybeInputFile agdaSource =
  withSystemTempDirectory "agda2html" $ \tempDir -> do

    -- We extract the module name and the module path from the Agda source.
    let moduleName = getModuleName agdaSource
    let modulePath = T.unpack <$> init (T.split (=='.') moduleName)

    -- Resolve the input file and the include path
    (inputFile, includePath) <-
      case maybeInputFile of
        -- If we've been given an input file, then we subtract the module names
        -- from the directories e.g., if we have been given src/Hello/World.lagda,
        -- which defines the module Hello.World, then we return ["src/"]
        Just inputFile -> do
          absInputFile <- makeAbsolute inputFile
          let
            absInputPath = takeDirectory absInputFile
            directories  = splitDirectories absInputPath
            includePath  = joinPath <$> stripSuffix directories modulePath
          -- note: if stripSuffix returns Nothing, this indicates a mismatch
          -- between the module names and the directory names; in this case,
          -- we will simply pass the inputPath as our include path, and
          -- forward Agda's error message to the user
          return (absInputFile, fromMaybe absInputPath includePath)

        -- If we've been given no input file, then we write the Agda source to a
        -- file called <Module>.lagda in the temporary directory, and return the
        -- path to that file, plus an empty include path.
        Nothing -> do
          let inputPath = tempDir </> joinPath modulePath
          let inputFile = inputPath </> T.unpack moduleName <.> "lagda"
          createDirectoryIfMissing True inputPath
          T.writeFile inputFile agdaSource
          return (inputFile, tempDir)

    let outputFile = tempDir </> T.unpack moduleName <.> "html"

    -- Call agda --html:
    let stdOutAndErr = if verbose then UseHandle stderr else CreatePipe

    (_, hout, herr, pid) <-
      createProcess_ "agda"
      ((proc "agda" [ "--allow-unsolved-metas"
                    , "--html"
                    , "--html-dir=" ++ tempDir
                    , "--include-path=" ++ includePath
                    , inputFile
                    ])
        { cwd     = Just tempDir
        , std_in  = NoStream
        , std_out = stdOutAndErr
        , std_err = stdOutAndErr })

    -- If agda does not fail:
    exitCode <- waitForProcess pid
    case exitCode of
      ExitSuccess -> do
        htmlSource <- T.readFile outputFile
        case useJekyll of
          Just jekyllRoot -> do
            localFiles <- agdaFilesIn includePath
            return $ liquidifyLocalHref jekyllRoot localFiles htmlSource
          Nothing ->
            return htmlSource

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
    rawOpen  | jekyll = "{% raw %}"    | otherwise = ""
    rawClose | jekyll = "{% endraw %}" | otherwise = ""

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


-- |Fix references to local modules.
liquidifyLocalHref :: FilePath -> [FilePath] -> T.Text -> T.Text
liquidifyLocalHref jekyllRoot paths =
  replaceAll (reLocal paths) . fromString $
    "\"{% endraw %}{{ site.baseurl }}{% link " <> jekyllRoot <> "$1.md %}{% raw %}$2\""

-- |An ICU regular expression which matches links to local files.
reLocal :: [FilePath] -> Regex
reLocal paths = regex [] hrefPatn
  where
    filePatn = T.concat . L.intersperse "|" . map (T.pack . takeBaseName) $ paths
    hrefPatn = "[\"'](" `T.append` filePatn `T.append` ")\\.html(#[^\"^']+)?[\"']"


-- |Correct references to the Agda stdlib.
correctStdLibHref :: IO (T.Text -> T.Text)
correctStdLibHref =
  reStdlibHref >>= \re ->
  return (replaceAll re "\"https://agda.github.io/agda-stdlib/$1.html$2\"")

-- |An ICU regular expression which matches links to the Agda stdlib.
reStdlibHref :: IO Regex
reStdlibHref = do
  modNames <- map T.pack <$> stdlibModules
  let
    builtin   = "Agda\\.[A-Za-z\\.]+"
    modPatns  = T.replace "." "\\." <$> modNames
    modPatn   = T.concat . L.intersperse "|" $ builtin : modPatns
    hrefPatn  = "[\"'](" `T.append` modPatn `T.append` ")\\.html(#[^\"^']+)?[\"']"
  return (regex [] hrefPatn)


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


-- | The 'stripSuffix' function drops the given suffix from a list.
-- It returns 'Nothing' if the list did not end with the suffix
-- given, or 'Just' the list before the suffix, if it does.
stripSuffix :: (Eq a) => [a] -> [a] -> Maybe [a]
stripSuffix xs ys = reverse <$> L.stripPrefix (reverse xs) (reverse ys)
