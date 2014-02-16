import qualified Data.ByteString.Char8 as B

import System.Fuse
import System.Environment
import System.Posix.Types

import Shiny.FS.Internal

import Shiny.Hardware (Hardware)
--import Shiny.Hardware.Dummy (mkDummyHardware)
import Shiny.Hardware.Serial (mkSerialHardware)

import Control.Applicative
import Control.Monad
import Control.Concurrent.MVar

type HT = ()

ledFSOps :: Hardware -> IO (FuseOperations HT)
ledFSOps hw = do
  tree <- mkFileTree hw
  mvar <- newMVar (FSState hw tree)
  return defaultFuseOps { fuseGetFileStat = ledGetFileStat mvar
                        , fuseOpen        = ledOpen mvar
                        , fuseRead        = ledRead mvar
                        , fuseWrite       = ledWrite mvar
                        , fuseSetFileSize = ledTruncate mvar
                        , fuseOpenDirectory = ledOpenDirectory mvar
                        , fuseReadDirectory = ledReadDirectory mvar
                        , fuseGetFileSystemStats = ledGetFileSystemStats mvar
                        }

ledGetFileStat :: MVar FSState -> FilePath -> IO (Either Errno FileStat)
ledGetFileStat mvar path = withMVar mvar $ \FSState{fileTree=tree} ->
  case (lookupPath path tree) of
    Just t   -> Right <$> (stat t)
    Nothing  -> return (Left eNOENT)

ledOpenDirectory :: MVar FSState -> FilePath -> IO (Errno)
ledOpenDirectory mvar path = withMVar mvar $ \FSState{fileTree=tree} ->
  case (lookupPath path tree) of
    Just Dir{} -> return eOK
    _          -> return eNOENT

ledReadDirectory :: MVar FSState -> FilePath -> IO (Either Errno [(FilePath, FileStat)])
ledReadDirectory mvar path = withMVar mvar $ \FSState{fileTree=tree} ->
  case (lookupPath path tree) of
    Just (Dir _ children) -> liftM Right $ mapM pathStat children
    _                     -> return (Left eNOENT)
  where pathStat child = do
          st <- stat child
          return (treeName child, st)

ledOpen :: MVar FSState -> FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno HT)
ledOpen mvar path _ _ = withMVar mvar $ \FSState{fileTree=tree} ->
  case (lookupPath path tree) of
    Just File {} -> return (Right ())
    _            -> return (Left eNOENT)

ledRead :: MVar FSState -> FilePath -> HT -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
ledRead mvar path _ count offset = withMVar mvar $ \FSState{fileTree=tree} ->
  case (lookupPath path tree) of
    Just f@File{} -> fileRead f count offset
    _              -> return (Left eNOENT)
    
ledWrite :: MVar FSState -> FilePath -> HT -> B.ByteString -> FileOffset -> IO (Either Errno ByteCount)
ledWrite mvar path _ dataIn offset = withMVar mvar $ \FSState{fileTree=tree} ->
  case (lookupPath path tree) of
    Just f@File{} -> fileWrite f dataIn offset
    _             -> return (Left eNOENT)
                                  
ledTruncate :: MVar FSState -> FilePath -> FileOffset -> IO Errno
ledTruncate _ _ _ = return eOK
                                    
ledGetFileSystemStats :: MVar FSState -> FilePath -> IO (Either Errno FileSystemStats)
ledGetFileSystemStats _ _ =
  return $ Right $ FileSystemStats
    { fsStatBlockSize = 512
    , fsStatBlockCount = 1
    , fsStatBlocksFree = 1
    , fsStatBlocksAvailable = 1
    , fsStatFileCount = 5
    , fsStatFilesFree = 10
    , fsStatMaxNameLength = 255
    }

main :: IO()
main = do
  progName <- getProgName
  args <- getArgs
  hw <- mkSerialHardware "/dev/ttyACM0" (8*40)
  fs <- ledFSOps hw
  fuseRun progName args fs defaultExceptionHandler
