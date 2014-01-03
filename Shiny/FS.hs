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

ledFSOps :: Hardware -> FuseOperations HT
ledFSOps hw = defaultFuseOps { fuseGetFileStat = ledGetFileStat tree
                             , fuseOpen        = ledOpen tree
                             , fuseRead        = ledRead tree
                             , fuseOpenDirectory = ledOpenDirectory tree
                             , fuseReadDirectory = ledReadDirectory tree
                             , fuseGetFileSystemStats = ledGetFileSystemStats tree
                            }
  where
    tree = mkFileTree hw

ledGetFileStat :: FileTree -> FilePath -> IO (Either Errno FileStat)
ledGetFileStat tree path = helper subTree
  where
    subTree = lookupPath path tree                         
    helper Nothing          = return (Left eNOENT)
    helper (Just (Dir _ _)) = getFuseContext >>= (return . Right . dirStat)
    helper (Just (File _ _ getSize)) = do
      ctx <- getFuseContext
      size <- getSize
      return $ Right $ fileStat ctx size

ledOpenDirectory tree "/" = return eOK
ledOpenDirectory tree _   = return eNOENT

ledReadDirectory :: FileTree -> FilePath -> IO (Either Errno [(FilePath, FileStat)])
ledReadDirectory tree path = helper (lookupPath path tree)
  where
    helper Nothing                   = return (Left eNOENT)
    helper (Just File{})             = return (Left eNOENT)
    helper (Just (Dir dName dTrees)) = liftM Right $ sequence $ map stat dTrees
    
    stat :: FileTree -> IO ((FilePath, FileStat))
    stat (Dir dName _) = do
      ctx <- getFuseContext
      return (dName, dirStat ctx)
    stat (File fName _ fSize) = do
      ctx <- getFuseContext
      size <- fSize
      return (fName, fileStat ctx size)

ledOpen :: FileTree -> FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno HT)
ledOpen tree path mode flags = helper (lookupPath path tree)
  where
    helper Nothing        = return (Left eNOENT)
    helper (Just Dir {})  = return (Left eNOENT)
    helper (Just File {}) = return (Right ())

ledRead :: FileTree -> FilePath -> HT -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
ledRead tree path _ count offset = helper (lookupPath path tree)
  where
    helper Nothing                 = return (Left eNOENT)
    helper (Just Dir{})            = return (Left eNOENT)
    helper (Just (File _ fRead _)) = liftM Right (fRead count offset)
    
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
  fuseRun progName args (ledFSOps hw) defaultExceptionHandler