module Main where

import Network.Wreq
import Control.Lens
import Text.HTML.Parser
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Lazy.Encoding as LTE (decodeUtf8)
import qualified Data.Text.Lazy.IO as IO (putStrLn,writeFile)
import qualified Data.Text as T 
import qualified Data.List as List (find,inits)
import Data.Maybe (fromJust)
import qualified Data.Text.Lazy as LT (append,pack,unpack)
import Data.Char (chr)


filterTest :: [(TagName,[Attr])]
filterTest = [
  (stringToTagName "div", stringToAttr "class" "article-header"),
  (stringToTagName "div", stringToAttr "class" "p-main-contents")]

filterTokens :: [(TagName,[Attr])] -> [Token] -> [Token]
filterTokens tagsTuples tokens = concat nestedList where
                           nestedList = filterFunction <*> (pure tokens) 
                           filterFunction = (map (uncurry $ filterToken) tagsTuples)

filterToken :: TagName -> [Attr] -> [Token] -> [Token]
filterToken tagname attr tokens = fromJust (List.find (\x -> mapping x == 0) withoutHead) where
                      mapping   = (foldr (countToken tagname) 0)
                      withoutHead = drop 1 $ List.inits dropped
                      dropped    = dropWhile ((/=) beginToken) tokens 
                      beginToken = TagOpen tagname attr
                      endToken   = TagClose tagname

countToken :: TagName -> Token -> Int -> Int
countToken tagname (TagOpen  tag _) x 
  | tag == tagname = x+1
countToken tagname (TagClose tag ) x 
  | tag == tagname = x-1
countToken _ _ x = x

stringToAttr :: String -> String -> [Attr]
stringToAttr attr values = [Attr (T.pack attr) (T.pack values)]

stringToTagName :: String -> TagName
stringToTagName str = T.pack str

--tupleToToken :: (TagName,[Attr])

spjPapers :: IO ()
spjPapers = do
  
        --let url = "http://research.microsoft.com/en-us/people/simonpj/"
        let url = "https://www.yomiuri.co.jp/national/20200128-OYT1T50285/"
        --let url = "https://www.yomiuri.co.jp/world/20200129-OYT1T50107/"
        --let url = "https://www.yomiuri.co.jp/hobby/atcars/impression/20200123-OYT8T50020/"
        response <- get url
        let body = response ^. responseBody :: L.ByteString
        let tokens = parseTokensLazy decodedBody where
                     decodedBody = LTE.decodeUtf8 body
        let parsedHtml = renderTokens article where 
                      article = filterTokens filterTest tokens 
        let unpacked = LT.unpack $ parsedHtml 
        --IO.writeFile "article3.html" parsedHtml
        putStrLn unpacked

main :: IO ()
main = spjPapers