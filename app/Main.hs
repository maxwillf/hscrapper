{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wreq
import Control.Lens
import Text.HTML.Parser
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Lazy.Encoding as LTE (decodeUtf8)
import qualified Data.Text.Lazy.IO as IO (putStrLn,writeFile)
import qualified Data.Text as T 
import qualified Data.Text.IO as TIO (putStrLn) 
import qualified Data.List as List (find,inits)
import Data.Maybe (fromJust)
import qualified Data.Text.Lazy as LT (append,pack,unpack)
import Data.Char (chr)


--(~==) :: Token -> Token -> Bool

getLinksFromTokens :: [Token] -> [T.Text]
getLinksFromTokens tokens = fromJust <$> (filter (\x -> x /= Nothing)) maybeLinks where
                     maybeLinks = getLinkFromAnchorToken <$> tokens

getLinkFromAnchorToken :: Token -> Maybe T.Text
getLinkFromAnchorToken (TagOpen tagname attrs)
  | tagname == "a" = hrefValue where
                            hrefValue = (\(Attr x y) -> y) <$> hrefAttr
                            hrefAttr = (List.find (\(Attr y z) -> y == "href") attrs)
getLinkFromAnchorToken _ = Nothing 

filterAnchors :: [Token] -> [Token]
filterAnchors tokens = filter isAnchor tokens

isAnchor :: Token -> Bool
isAnchor (TagOpen tagname attrs) = tagname == "a"
isAnchor _ = False

articlesFilterTags :: [(TagName,[Attr])]
articlesFilterTags = [
  ("div", [Attr "class" "article-header"]),
  ("div", [Attr "class" "p-main-contents"])]

crawlingFilterTags :: [(TagName,[Attr])]
crawlingFilterTags = [
  ("li", [Attr "class" "p-list-item  "]),
  ("li", [Attr "class" "p-list-item p-list-item--small-thumb "])]

filterTokens :: [(TagName,[Attr])] -> [Token] -> [Token]
filterTokens tagsTuples tokens = concat nestedList where
                           nestedList = filterFunction <*> (pure tokens) 
                           filterFunction = (map (uncurry $ filterToken) tagsTuples)

-- tratar erro aqui, caso a tag nao seja encontrada o programa irá retornar uma seção 
-- com a função fromJust
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

responseToTokens :: Response L.ByteString -> [Token]
responseToTokens response = parsedTokens where
                  parsedTokens = parseTokensLazy decodedBody
                  decodedBody = LTE.decodeUtf8 (response ^. responseBody)

responseToFilteredTokens ::  [(TagName, [Attr])] -> Response L.ByteString -> [Token]
responseToFilteredTokens filterTags response  = 
                  filterTokens filterTags (responseToTokens response)

main :: IO ()
main = do

        let crawlingPath = "https://www.yomiuri.co.jp/news/" 
        response <- get crawlingPath
        let filteredTokens = (responseToFilteredTokens crawlingFilterTags response)
        let crawlingLinks = getLinksFromTokens filteredTokens
        mapM_ TIO.putStrLn crawlingLinks
        -- IO.putStrLn (renderTokens filteredTokens)
        let responses = get <$> (map T.unpack crawlingLinks)
        let scrapped = map (fmap (responseToFilteredTokens articlesFilterTags )) responses
        let rendered = map (fmap (renderTokens)) scrapped
        IO.putStrLn <$> (head rendered)
        mapM_ (fmap IO.putStrLn) rendered
        return ()
        -- mapM_ IO.putStrLn responses
        --let url = "http://research.microsoft.com/en-us/people/simonpj/"
        -- let url = "https://www.yomiuri.co.jp/national/20200128-OYT1T50285/"
        --let url = "https://www.yomiuri.co.jp/world/20200129-OYT1T50107/"
        --let url = "https://www.yomiuri.co.jp/hobby/atcars/impression/20200123-OYT8T50020/"
        -- response <- get url
        -- let body = response ^. responseBody :: L.ByteString
        -- let tokens = parseTokensLazy decodedBody where
        --              decodedBody = LTE.decodeUtf8 body
        -- let parsedHtml = renderTokens article where 
        --               article = filterTokens articlesFilterTags tokens 
        -- let unpacked = LT.unpack $ parsedHtml 
        -- --IO.writeFile "article3.html" parsedHtml
        -- putStrLn unpacked

-- stringToAttr :: String -> String -> [Attr]
-- stringToAttr attr values = [Attr (T.pack attr) (T.pack values)]

-- stringToTagName :: String -> TagName
-- stringToTagName str = T.pack str