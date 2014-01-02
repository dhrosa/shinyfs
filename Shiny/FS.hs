import qualified Data.ByteString.Char8 as B

import System.Fuse
import System.Posix.Types
import System.Posix.Files
import System.Posix.IO

type HT = ()

ledFSOps :: FuseOperations HT
ledFSOps = defaultFuseOps { fuseGetFileStat = ledGetFileStat
                            , fuseOpen        = ledOpen
                            , fuseRead        = ledRead 
                            , fuseOpenDirectory = ledOpenDirectory
                            , fuseReadDirectory = ledReadDirectory
                            , fuseGetFileSystemStats = ledGetFileSystemStats
                            }
helloString :: B.ByteString
helloString = B.pack "Hello World, HFuse!\n"

helloPath :: FilePath
helloPath = "/hello"
dirStat ctx = FileStat { statEntryType = Directory
                       , statFileMode = foldr1 unionFileModes
                                          [ ownerReadMode
                                          , ownerExecuteMode
                                          , groupReadMode
                                          , groupExecuteMode
                                          , otherReadMode
                                          , otherExecuteMode
                                          ]
                       , statLinkCount = 2
                       , statFileOwner = fuseCtxUserID ctx
                       , statFileGroup = fuseCtxGroupID ctx
                       , statSpecialDeviceID = 0
                       , statFileSize = 4096
                       , statBlocks = 1
                       , statAccessTime = 0
                       , statModificationTime = 0
                       , statStatusChangeTime = 0
                       }

fileStat ctx = FileStat { statEntryType = RegularFile
                        , statFileMode = foldr1 unionFileModes
                                           [ ownerReadMode
                                           , groupReadMode
                                           , otherReadMode
                                           ]
                        , statLinkCount = 1
                        , statFileOwner = fuseCtxUserID ctx
                        , statFileGroup = fuseCtxGroupID ctx
                        , statSpecialDeviceID = 0
                        , statFileSize = fromIntegral $ B.length helloString
                        , statBlocks = 1
                        , statAccessTime = 0
                        , statModificationTime = 0
                        , statStatusChangeTime = 0
                        }

ledGetFileStat :: FilePath -> IO (Either Errno FileStat)
ledGetFileStat "/" = do
    ctx <- getFuseContext
    return $ Right $ dirStat ctx
ledGetFileStat path | path == helloPath = do
    ctx <- getFuseContext
    return $ Right $ fileStat ctx
ledGetFileStat _ =
    return $ Left eNOENT

ledOpenDirectory "/" = return eOK
ledOpenDirectory _   = return eNOENT

ledReadDirectory :: FilePath -> IO (Either Errno [(FilePath, FileStat)])
ledReadDirectory "/" = do
    ctx <- getFuseContext
    return $ Right [(".",          dirStat  ctx)
                   ,("..",         dirStat  ctx)
                   ,(helloName,    fileStat ctx)
                   ]
    where (_:helloName) = helloPath
ledReadDirectory _ = return (Left (eNOENT))

ledOpen :: FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno HT)
ledOpen path mode flags
    | path == helloPath = case mode of
                            ReadOnly -> return (Right ())
                            _        -> return (Left eACCES)
    | otherwise         = return (Left eNOENT)


ledRead :: FilePath -> HT -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
ledRead path _ byteCount offset
    | path == helloPath =
        return $ Right $ B.take (fromIntegral byteCount) $ B.drop (fromIntegral offset) helloString
    | otherwise         = return $ Left eNOENT

ledGetFileSystemStats :: String -> IO (Either Errno FileSystemStats)
ledGetFileSystemStats str =
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
  fuseMain ledFSOps defaultExceptionHandler