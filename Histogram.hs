module Histogram where
  
import Data.Word
import Data.ByteString as BS
import Data.Map
import qualified Data.Map.Strict as M

buildHistogram :: IO ()
buildHistogram  = 
  do
      byteString <- BS.readFile "dcode-file"
      let statistic = buildStatistic byteString
      print $ Data.Map.toList statistic

countByte :: (Ord k, Num a) => Map k a -> k -> Map k a
countByte oldMap b = M.insertWith (+) b 1 oldMap

buildStatistic :: ByteString -> Map Word8 Integer
buildStatistic = BS.foldl countByte Data.Map.empty