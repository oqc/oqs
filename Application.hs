{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    ) where

import Import
import Settings
import Yesod.Auth
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
import Network.Wai.Middleware.RequestLogger
import qualified Database.Persist
import Database.Persist.Sql (runMigration)
import Network.HTTP.Conduit (newManager, def)
import Control.Monad.Logger (runLoggingT)
import System.IO (stdout)
import System.Log.FastLogger (mkLogger)
-- non scaffoled imports below
import System.Directory (getDirectoryContents)
import Data.Text (unpack, dropWhileEnd)
import Data.Text.Encoding (decodeUtf8)
import Data.IORef (newIORef)
import Data.List (isSuffixOf, head)
import System.FilePath (pathSeparator)
import qualified Data.Map as M
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as B64
import Data.List.Split (chunksOf)
import Numeric (readHex)


-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Home
import Handler.RefString
import Handler.GithubWebHook

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: AppConfig DefaultEnv Extra -> IO Application
makeApplication conf = do
    foundation <- makeFoundation conf

    -- Initialize the logging middleware
    logWare <- mkRequestLogger def
        { outputFormat =
            if development
                then Detailed True
                else Apache FromSocket
        , destination = Logger $ appLogger foundation
        }

    -- Create the WAI application and apply middlewares
    app <- toWaiAppPlain foundation
    return $ logWare app

-- | Loads up any necessary settings, creates your foundation datatype, and
-- performs some initialization.
makeFoundation :: AppConfig DefaultEnv Extra -> IO App
makeFoundation conf = do
    let qdfPath = unpack $ extraQdfPath (appExtra conf)
    manager <- newManager def
    static  <- staticSite
    fns     <- getDirectoryContents qdfPath
    qtfMap  <- localFilesToMap readQtfFiles qdfPath ".qtf" fns >>= newIORef
    qpfMap  <- localFilesToMap readQpfFiles qdfPath ".qpf" fns >>= newIORef
    dbCnf   <- withYamlEnvironment "config/sqlite.yml" (appEnv conf)
               Database.Persist.loadConfig >>=
               Database.Persist.applyEnv
    poolCnf <- Database.Persist.createPoolConfig (dbCnf :: Settings.PersistConf)
    logger  <- mkLogger True stdout
    let foundation = App manager logger conf static qtfMap qpfMap dbCnf poolCnf
    -- Perform database migration using our application's logging settings.
    runLoggingT
        (Database.Persist.runPool dbCnf (runMigration migrateAll) poolCnf)
        (messageLoggerSource foundation logger)
    return foundation
    where
      localFilesToMap :: ([FilePath] -> IO [QLines a]) -> FilePath -> String -> [FilePath] -> IO (M.Map Text (QLines a))
      localFilesToMap filesReader qdfPath ext fns =
        let sfns          = filter (isSuffixOf ext) fns in
        let toPaths       = map ((++) $ qdfPath ++ [pathSeparator]) in
        let fileNameToID  = (dropWhileEnd (== '=')) . decodeUtf8 . B64.encode . BS.pack .
                            (map (fst . head . readHex)) . (chunksOf 2 . take 16) in
        {- let fileNameToID = decodeUtf8 . B64.encode . toLazyByteString . -- might work from bytestring-0.10.x -}
        {-                    (foldr (<>) mempty) . (map (word8 . fromIntegral . digitToInt)) . (take 16) in-}
        filesReader (toPaths sfns) >>= return . M.fromList . (zip (map fileNameToID sfns))


-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader makeApplication
  where
    loader = Yesod.Default.Config.loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }
