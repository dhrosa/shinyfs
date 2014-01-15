module Shiny.FS.Internal where

import qualified Data.ByteString.Char8 as B

import System.Fuse
import System.Posix.Types
import System.Posix.Files hiding (fileSize)

import Control.Monad
import Data.List (find)
import Data.Maybe

import Shiny.Shiny
import Shiny.Hardware

import Text.Printf (printf)
import Data.List.Split (chunksOf)
import qualified Numeric as N

import Shiny.Focus

data FileTree = File
                {
                  fileName  :: String,
                  fileRead  :: ByteCount -> FileOffset -> IO (Either Errno B.ByteString),
                  fileWrite :: B.ByteString -> FileOffset -> IO (Either Errno ByteCount),
                  fileSize  :: IO (Int)
                }
              | Dir
                {
                  dirName     :: String,
                  dirChildren :: [FileTree]
                }
  
instance Show FileTree where
  show File {fileName = name}  = "File: " ++ name
  show (Dir name children) = "Dir: " ++ name ++ " " ++ (show $ map treeName children)
                
-- | Constructs the file system from the given hardware
mkFileTree :: Hardware -> IO (FileTree)
mkFileTree hw = do
  numLeds <- displaySize hw
  return $ Dir "/" $ [countFile numLeds,  Dir "leds" (map (ledDir hw focusAll numLeds) [0..numLeds-1])]
  where
    focusAll = onIndices (const True)

-- | Adds a parent tree to a dir
addChild :: FileTree -> FileTree -> FileTree
addChild child (Dir name children) = Dir name (child:children)
addChild _ _                       = error "Cannot add children to a file."

-- | Constructs file with a given name and no contents
emptyFile :: String -> FileTree
emptyFile name = File name emptyRead emptyWrite emptySize
  where
    emptyRead _ _  = return (Right B.empty)
    emptyWrite _ _ = return (Left eACCES)
    emptySize = return 0

-- | File representing the number of LEDs in the display
countFile :: Int -> FileTree
countFile size = File "count" countRead countWrite countSize
  where
    countRead _ _  = return $ Right $ B.pack (show size)
    countWrite _ _ = return (Left eACCES)
    countSize      = return (length (show size))

-- | File for viewing a subset of the display as newline separated hex triplets
hexFile :: Hardware -> Focus -> FileTree
hexFile hw focus = File "hex" readHex writeHex sizeHex
  where
    toHex (RGB r g b)    = printf "%02x%02x%02x" r g b
    getFocusedDisplay    = liftM (focusOn focus) (readDisplay hw)
    
    readHex count offset = do
      text <- liftM (unlines . map toHex) getFocusedDisplay
      let subText = take (fromIntegral count) . drop (fromIntegral offset) $ text
      return (Right . B.pack $ subText)
      
    writeHex inData coffset = do
      oldDisp <- readDisplay hw
      let
        oldFocusedDisp = focusOn focus oldDisp
        offset     = fromIntegral coffset
        oldStr     = B.pack . unlines . map toHex $ oldFocusedDisp
        -- Convert original data to string, splice in input string, attempt to parse the new string
        -- This method allows us to easily handle arbitrary offsets, but is also inefficient
        -- TODO: allow unaligned writing without loading the entire array
        splicedStr = B.concat [B.take offset oldStr,
                               inData,
                               B.drop (offset + B.length inData) oldStr]
        parsedLEDs = map (readLEDHex . init) . chunksOf 7 . B.unpack $ splicedStr
        newDisp    = replace focus (catMaybes parsedLEDs) oldDisp
      if (all isJust parsedLEDs)
        then (updateDisplay hw newDisp) >> (return . Right . fromIntegral . B.length $ inData)
        else return . Left $ eINVAL
        
    sizeHex                = liftM ((7*) . length) getFocusedDisplay


-- | Attempts to parse an LED from a string of 6 hex digits
readLEDHex :: String -> Maybe LED
readLEDHex str
  | all success parsed = case (map (fst.head) parsed) of
      [r, g, b] -> Just (RGB r g b)
      _         -> Nothing
  | otherwise    = Nothing
  where
    parsed = map N.readHex (chunksOf 2 str)
    success [(_, rest)] = null rest
    success _           = False
        
-- | TODO
ledDir :: Hardware -> Focus -> Int -> Int -> FileTree
ledDir hw focus numLeds n = Dir (show n) [Dir "to" toDirs]
  where
    toDirs = map subLedDir [n..numLeds-1]
    subLedDir m = Dir (show m) [hexFile hw (range n (m+1))]

-- | The name of a file or directory
treeName :: FileTree -> String
treeName File{fileName = fName} = fName
treeName Dir{dirName = dName}   = dName

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
lookupPath _ _ = Nothing

-- | File status
stat :: FileTree -> IO (FileStat)

stat Dir{} = do
  ctx <- getFuseContext
  return $ FileStat { statEntryType = Directory
                       , statFileMode = foldr1 unionFileModes
                                          [ ownerReadMode
                                          , ownerWriteMode
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

stat File {fileSize = fSize} = do
  ctx <- getFuseContext
  size <- fSize
  return $ FileStat { statEntryType = RegularFile
                              , statFileMode = foldr1 unionFileModes
                                               [ ownerReadMode
                                               , ownerWriteMode
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