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
                  dirName  :: String,
                  dirTrees :: [FileTree]
                }
  
instance Show FileTree where
  show (File name _ _) = "File: " ++ name
  show (Dir name trees) = "Dir: " ++ name ++ " " ++ (show $ map treeName trees)
                
-- | Constructs the file system from the given hardware
mkFileTree :: Hardware -> FileTree
mkFileTree hw = Dir "/" $ [countFile hw]

-- | File representing the number of LEDs in the dispaly
countFile :: Hardware -> FileTree
countFile hw = File "count" countRead countSize
  where
    countRead _ _ = liftM (B.pack . show) (displaySize hw)
    countSize = liftM (length . show) (displaySize hw)

-- | The name of a file or directory
treeName :: FileTree -> String
treeName (File fName _ _) = fName
treeName (Dir dName _) = dName

isFile :: FileTree -> Bool
isFile (File _ _ _) = True
isFile _ = False

isDir :: FileTree -> Bool
isDir = not . isFile

lookupPath :: String -> FileTree -> Maybe (FileTree)
lookupPath "/" tree
  | isDir tree = Just tree
  | otherwise = Nothing
lookupPath ('/':path) tree
  | null subPath = subTree
  | otherwise    = subTree >>= (lookupPath subPath)
  where
    subTree = find ((==name) . treeName) (dirTrees tree)
    (name, subPath) = span (/= '/') path

stat :: FileTree -> IO (FileStat)

stat (Dir _ _) = do
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