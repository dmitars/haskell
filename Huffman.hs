module Huffman where
import Data.Map
import Data.List
import qualified Data.Map.Strict as M


data HuffmanTree = Leaf Char Integer
                 | SubTree HuffmanTree HuffmanTree Integer
                 deriving(Show)


doHuffman :: IO ()
doHuffman  =
  do
      putStrLn "Enter input file name:"
      inputFileName <- getLine
      putStrLn "Enter output file name:"
      outputFileName <- getLine
      text <- readFile inputFileName
      let tree = buildTree (buildStatistic text)
      let codes = buildCodes tree
      print $ Data.Map.toList codes
      let encoded = encodeText text codes
      putStrLn encoded
      let decoded = decodeText tree encoded
      putStrLn decoded
      writeFile outputFileName (show codes)
      writeFile outputFileName encoded
      

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
     where  mergeSubTrees [t]    = t
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