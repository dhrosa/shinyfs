module Shiny.FS.Internal where

import qualified Data.ByteString.Char8 as B

import System.Fuse
import System.Posix.Types
import System.Posix.Files hiding (fileSize)
import System.Posix.IO

import Control.Monad
import Data.List (find)

import Shiny.Hardware
  
data FileTree = File
                {
                  fileName  :: String,
                  fileRead  :: ByteCount -> FileOffset -> IO (B.ByteString),
                  fileSize  :: IO (Int)
                }
              | Dir
                {
                  dirName     :: String,
                  dirChildren :: [FileTree]
                }
  
instance Show FileTree where
  show (File name _ _) = "File: " ++ name
  show (Dir name trees) = "Dir: " ++ name ++ " " ++ (show $ map treeName trees)
                
-- | Constructs the file system from the given hardware
mkFileTree :: Hardware -> IO (FileTree)
mkFileTree hw = do
  numLeds <- displaySize hw
  let ledDir n = Dir (show n) [emptyFile "color"]
  return $ Dir "/" $ [countFile numLeds, Dir "leds" (map ledDir [0..numLeds-1])]

-- | Adds a parent tree to a dir
addChild :: FileTree -> FileTree -> FileTree
addChild child (Dir name children) = Dir name (child:children)
addChild _ _                       = error "Cannot add children to a file."

-- | Constructs file with a given name and no contents
emptyFile :: String -> FileTree
emptyFile name = File name emptyRead emptySize
  where
    emptyRead _ _ = return B.empty
    emptySize = return 0

-- | File representing the number of LEDs in the display
countFile :: Int -> FileTree
countFile size = File "count" countRead countSize
  where
    countRead _ _ = return (B.pack (show size))
    countSize     = return (length (show size))

-- | The name of a file or directory
treeName :: FileTree -> String
treeName (File fName _ _) = fName
treeName (Dir dName _) = dName

-- | Is this tree a file?
isFile :: FileTree -> Bool
isFile File{} = True
isFile _ = False

-- | Is this tree a directory?
isDir :: FileTree -> Bool
isDir = not . isFile

-- | Uses the given path to retrieve a descendant of the tree
lookupPath :: String -> FileTree -> Maybe (FileTree)
lookupPath "/" tree
  | isDir tree = Just tree
  | otherwise  = Nothing
lookupPath ('/':path) tree
  | null subPath = child
  | otherwise    = child >>= (lookupPath subPath)
  where
    child = find ((==name) . treeName) (dirChildren tree)
    (name, subPath) = span (/= '/') path

-- | File status
stat :: FileTree -> IO (FileStat)

stat Dir{} = do
  ctx <- getFuseContext
  return $ FileStat { statEntryType = Directory
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

stat (File _ _ fSize) = do
  ctx <- getFuseContext
  size <- fSize
  return $ FileStat { statEntryType = RegularFile
                              , statFileMode = foldr1 unionFileModes
                                               [ ownerReadMode
                                               , groupReadMode
                                               , otherReadMode
                                               ]
                              , statLinkCount = 1
                              , statFileOwner = fuseCtxUserID ctx
                              , statFileGroup = fuseCtxGroupID ctx
                              , statSpecialDeviceID = 0
                              , statFileSize = fromIntegral size
                              , statBlocks = 1
                              , statAccessTime = 0
                              , statModificationTime = 0
                              , statStatusChangeTime = 0
                              }