module Lib.IO (
  inputFileList,
  puts
) where

import qualified System.Directory
import qualified Data.Text

inputFileList :: IO [Data.Text.Text]
inputFileList = do
  cwd <- System.Directory.getCurrentDirectory
  list <- System.Directory.listDirectory cwd
  let mlist = map Data.Text.pack list
  let flist = filter isTextFilename mlist
  return flist


isTextFilename :: Data.Text.Text -> Bool
isTextFilename = Data.Text.isSuffixOf (Data.Text.pack ".txt")

puts :: Data.Text.Text -> IO ()
puts = putStrLn . Data.Text.unpack