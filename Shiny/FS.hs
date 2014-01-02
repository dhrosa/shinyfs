import qualified Data.ByteString.Char8 as B

import System.Fuse
import System.Environment
import System.Posix.Types
import System.Posix.Files
import System.Posix.IO

import Shiny.FS.Internal

import Shiny.Hardware (Hardware)
import Shiny.Hardware.Dummy (mkDummyHardware)

type HT = ()

topLevelFiles = [
  "status",
  "reset",
  "setall",
  "count"
  ]

data FSState = FSState
                 {
                   hardware :: Hardware
                 }

ledFSOps :: Hardware -> FuseOperations HT
ledFSOps hw = defaultFuseOps { fuseGetFileStat = ledGetFileStat state
                             , fuseOpen        = ledOpen state
                             , fuseRead        = ledRead state
                             , fuseOpenDirectory = ledOpenDirectory state
                             , fuseReadDirectory = ledReadDirectory state
                             , fuseGetFileSystemStats = ledGetFileSystemStats state
                            }
  where
    state = FSState hw

ledGetFileStat :: FSState -> FilePath -> IO (Either Errno FileStat)
ledGetFileStat state "/" = getFuseContext >>= (return . Right . dirStat)
ledGetFileStat (FSState hw) (_:path)
  | path `elem` topLevelFiles = getFuseContext >>= (return . Right . fileStat)
  | otherwise = return $ Left eNOENT

ledOpenDirectory state "/" = return eOK
ledOpenDirectory state _   = return eNOENT

ledReadDirectory :: FSState -> FilePath -> IO (Either Errno [(FilePath, FileStat)])
ledReadDirectory _ "/" = do
    ctx <- getFuseContext
    return $ Right $ [(".",          dirStat  ctx)
                     ,("..",         dirStat  ctx)
                     ] ++ zip topLevelFiles (repeat (fileStat ctx))
ledReadDirectory _ _ = return (Left (eNOENT))

ledOpen :: FSState -> FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno HT)
ledOpen _ (_:path) mode flags
  | path `elem` topLevelFiles = return (Right ())
  | otherwise                 = return (Left eNOENT)


ledRead :: FSState -> FilePath -> HT -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
ledRead _ (_:"count") _ _ _ = (return . Right . B.pack . show) 1024
ledRead _ _ _ _ _ = return $ Left eNOENT

ledGetFileSystemStats :: FSState -> String -> IO (Either Errno FileSystemStats)
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