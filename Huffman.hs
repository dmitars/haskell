module Huffman where
import Data.Map
import Data.List
import qualified Data.Map.Strict as M


data HuffmanTree = Leaf Char Integer
                 | SubTree HuffmanTree HuffmanTree Integer
                 deriving(Show)

showStatisticPair :: (Char,Integer) -> String
showStatisticPair (symbol,weight) = [symbol] ++ " " ++ show weight

doHuffman :: IO ()
doHuffman  =
  do
      putStrLn "Enter input file name:"
      inputFileName <- getLine
      putStrLn "Enter output file name:"
      outputFileName <- getLine
      putStrLn "Enter y to encode or any another symbol to decode"
      input <- getLine
      if head input == 'y'
      then encode inputFileName outputFileName
      else decode inputFileName outputFileName


encode :: String -> String -> IO()
encode inputFileName outputFileName =
  do
      text <- readFile inputFileName
      let statistic = buildStatistic text
      let tree = buildTree statistic
      let codes = buildCodes tree
      let encoded = encodeText text codes
      writeEncoded statistic encoded outputFileName
      putStrLn "Encoding done!"


decode :: String -> String -> IO()
decode inputFileName outputFileName =
  do
     (statistic, encoded) <- readEncoded inputFileName
     let tree = buildTree statistic
     let decoded = decodeText tree encoded
     writeFile outputFileName decoded
     putStrLn "Decoding done!"


writeEncoded :: Map Char Integer -> [Char] -> FilePath -> IO ()
writeEncoded statistic encoded outputFileName
 = writeFile outputFileName (intercalate "\n" (Prelude.map showStatisticPair (toAscList statistic)) ++ "\n" ++ encoded)
 
readEncoded :: String -> IO (Map Char Integer, String)
readEncoded encodedFileName = buildEncoded <$> (Data.List.reverse <$> readWordsFrom encodedFileName)
        where buildEncoded (encoded:statisticWords) = (buildIntStringMapFrom statisticWords, encoded)
              buildEncoded [] = (Data.Map.empty, [])
  
readWordsFrom :: String -> IO [String]
readWordsFrom fileName = words <$> readFile fileName

getSymbolFromWord :: String -> Char
getSymbolFromWord word = if length word > 1 then errorWithoutStackTrace "Illegal symbol format in statistic"
                                            else head word  
  
buildIntStringMapFrom :: [String] -> Map Char Integer
buildIntStringMapFrom statisticWords = Data.Map.fromList (buildPairsArray statisticWords)
                      where buildPairsArray :: [String] -> [(Char,Integer)]
                            buildPairsArray [[]] = []
                            buildPairsArray [] = []
                            buildPairsArray [_:_] = errorWithoutStackTrace "Illegal input file format: illegal statistic format or encoded text not present"
                            buildPairsArray (value:key:anotherStatisticWords) = [(getSymbolFromWord key, read value :: Integer)] ++ buildPairsArray anotherStatisticWords


weightOf :: HuffmanTree -> Integer
weightOf (Leaf _ weight) = weight
weightOf (SubTree _ _ weight) = weight

merge :: HuffmanTree -> HuffmanTree -> HuffmanTree
merge firstTree secondTree = SubTree firstTree secondTree (weightOf firstTree + weightOf secondTree)


countChar :: (Ord k, Num a) => Map k a -> k -> Map k a
countChar oldMap b = M.insertWith (+) b 1 oldMap

buildStatistic :: String -> Map Char Integer
buildStatistic = Prelude.foldl countChar Data.Map.empty


comparator :: HuffmanTree -> HuffmanTree -> Ordering
comparator firstLeaf secondLeaf | weightOf firstLeaf < weightOf secondLeaf = LT
                                | otherwise = GT

buildTree :: Map Char Integer -> HuffmanTree
buildTree statistic = mergeSubTrees (Prelude.map (uncurry Leaf) (Data.Map.toAscList statistic))
     where  mergeSubTrees [] = errorWithoutStackTrace "Symbols statistic in file with encoded text is empty"
            mergeSubTrees [t]    = t
            mergeSubTrees (a:b:cs)  = mergeSubTrees $ insertBy comparator (merge a b) cs

            
buildCodes :: HuffmanTree -> Map Char [Char]
buildCodes  = fromList . buildCodeList
  where buildCodeList (Leaf symbol _) = [(symbol, [])]
        buildCodeList (SubTree leftChild rightChild _) = Prelude.map (addSymbol '0') (buildCodeList leftChild) ++ Prelude.map (addSymbol '1') (buildCodeList rightChild)
                                                          where addSymbol symbol (key, value) = (key, symbol:value)
        
        
encodeText :: String -> Map Char [Char] -> [Char]
encodeText [] _ = []
encodeText (symbol:text) codes = codes ! symbol ++ encodeText text codes

decodeText :: HuffmanTree -> [Char] -> String
decodeText tree = findLeaf tree
    where findLeaf (Leaf symbol _) [] = [symbol]
          findLeaf (Leaf symbol _) path = symbol : findLeaf tree path
          findLeaf (SubTree leftChild rightChild _) (lastSymbol:path) = findLeaf (if lastSymbol == '0' then leftChild else rightChild) path