import qualified Data.ByteString.Char8 as B

import System.Fuse
import System.Environment
import System.Posix.Types
import System.Posix.Files hiding (fileSize)
import System.Posix.IO

import Shiny.FS.Internal

import Shiny.Hardware (Hardware)
import Shiny.Hardware.Dummy (mkDummyHardware)

import Control.Monad

type HT = ()

ledFSOps :: Hardware -> IO (FuseOperations HT)
ledFSOps hw = do
  tree <- mkFileTree hw
  return defaultFuseOps { fuseGetFileStat = ledGetFileStat tree
                        , fuseOpen        = ledOpen tree
                        , fuseRead        = ledRead tree
                        , fuseOpenDirectory = ledOpenDirectory tree
                        , fuseReadDirectory = ledReadDirectory tree
                        , fuseGetFileSystemStats = ledGetFileSystemStats tree
                        }

ledGetFileStat :: FileTree -> FilePath -> IO (Either Errno FileStat)
ledGetFileStat tree path = helper (lookupPath path tree)
  where
    helper (Just t) = liftM Right (stat t)
    helper Nothing  = return (Left eNOENT)

ledOpenDirectory tree path = helper (lookupPath path tree)
  where
    helper (Just Dir{}) = return eOK
    helper _            = return eNOENT

ledReadDirectory :: FileTree -> FilePath -> IO (Either Errno [(FilePath, FileStat)])
ledReadDirectory tree path = helper (lookupPath path tree)
  where
    helper (Just (Dir _ children)) = liftM Right $ mapM pathStat children
    helper _                       = return (Left eNOENT)
    pathStat child = do
      st <- stat child
      return (treeName child, st)

ledOpen :: FileTree -> FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno HT)
ledOpen tree path mode flags = helper (lookupPath path tree)
  where
    helper (Just File {}) = return (Right ())
    helper _             = return (Left eNOENT)

ledRead :: FileTree -> FilePath -> HT -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
ledRead tree path _ count offset = helper (lookupPath path tree)
  where
    helper (Just (File _ fRead _)) = liftM Right (fRead count offset)
    helper _                      = return (Left eNOENT)
    
ledGetFileSystemStats :: FileTree -> String -> IO (Either Errno FileSystemStats)
ledGetFileSystemStats _ str =
  return $ Right $ FileSystemStats
    { fsStatBlockSize = 512
    , fsStatBlockCount = 1
    , fsStatBlocksFree = 1
    , fsStatBlocksAvailable = 1
    , fsStatFileCount = 5
    , fsStatFilesFree = 10
    , fsStatMaxNameLength = 255
    }

main = do
  progName <- getProgName
  args <- getArgs
  hw <- mkDummyHardware 8
  fs <- ledFSOps hw
  fuseRun progName args fs defaultExceptionHandler