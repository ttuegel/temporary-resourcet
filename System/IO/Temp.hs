module System.IO.Temp
    ( createTempDirectory
    , openBinaryTempFile
    , openTempFile
    ) where

-- NB: this module was extracted directly from "Distribution/Simple/Utils.hs"
-- in a Cabal tree whose most recent commit was on Sun Oct 10 22:00:26
--
-- The files in the Distribution/Compat tree are exact copies of the
-- corresponding file in the Cabal checkout.


import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Resource (MonadResource, ReleaseKey, allocate)
import System.Directory
        ( doesDirectoryExist, doesFileExist, getTemporaryDirectory
        , removeDirectory, removeFile )
import System.IO (Handle)

import qualified Distribution.Compat.TempFile as Compat

removeDirectoryIfExists :: FilePath -> IO ()
removeDirectoryIfExists path = do
    exists <- doesDirectoryExist path
    when exists $ removeDirectory path

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists path = do
    exists <- doesFileExist path
    when exists $ removeFile path

resolveTempDir :: MonadIO m => Maybe FilePath -> m FilePath
resolveTempDir = maybe (liftIO getTemporaryDirectory) return

createTempDirectory :: MonadResource m
                    => Maybe FilePath -> String -> m (ReleaseKey, FilePath)
createTempDirectory mDir tmpl = do
    dir <- resolveTempDir mDir
    allocate (Compat.createTempDirectory dir tmpl) removeDirectoryIfExists

openBinaryTempFile :: MonadResource m
                   => Maybe FilePath -> String
                   -> m (ReleaseKey, FilePath, Handle)
openBinaryTempFile mDir tmpl = do
    dir <- resolveTempDir mDir
    (key, (path, h)) <- allocate (Compat.openBinaryTempFile dir tmpl)
                                 (removeFileIfExists . fst)
    return (key, path, h)

openTempFile :: MonadResource m
             => Maybe FilePath -> String -> m (ReleaseKey, FilePath, Handle)
openTempFile mDir tmpl = do
    dir <- resolveTempDir mDir
    (key, (path, h)) <- allocate (Compat.openTempFile dir tmpl)
                                 (removeFileIfExists . fst)
    return (key, path, h)
