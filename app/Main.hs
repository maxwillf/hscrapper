{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import GHC.Generics
import Network.Wreq
import Control.Lens
import Text.HTML.Parser
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Lazy.Encoding as LTE (encodeUtf8,decodeUtf8)
import qualified Data.Text.Lazy.IO as IO (putStrLn,writeFile)
import qualified Data.Text as T 
import qualified Data.Text.IO as TIO (putStrLn) 
import qualified Data.List as List (find,inits,tails,findIndex,zip)
import Data.Maybe (fromJust)
import qualified Data.Text.Lazy as LT (append,pack,unpack,toStrict)
import Data.Char (chr)

import Data.Yaml


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
  -- ("h3" , [Attr "class" "c-list-title"])
    ("li", [Attr "class" "p-list-item  "]),
    ("li", [Attr "class" "p-list-item p-list-item--small-thumb "])
   ]

filterTokens :: [(TagName,[Attr])] -> [Token] -> [Token]
filterTokens tagsTuples tokens = concat nestedList  where
                           nestedList = filterFunction <*> (pure tokens) 
                           filterFunction = (map (uncurry $ filterToken) tagsTuples)

filterToken :: TagName -> [Attr] -> [Token] -> [Token]
filterToken tagname attr tokens = case (List.findIndex (\x -> mapping x == 0) withoutHead) of
                      Just x -> (withoutHead !! x) ++ (filterToken tagname attr $ tokenTails !! x)
                      Nothing -> [] 
                      where
                      mapping   = (foldr (countToken tagname) 0)
                      tokenTails = drop 1 $ List.tails dropped
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

------------------------------------- yaml parsing variables

-- data Attribute = Attribute String String
-- data Tag = Tag {

-- }
-- data Config = Config {
--   domain :: String,
--   scrapperItens :: [Tag]
-- } deriving (Show,Generic)

data Tag = Tag {
  tag  :: String,
  attrName :: String,
  attrVal :: String
} deriving (Show,Generic)

data Config = Config {
  domain :: String,
  scrapperItens :: [Tag]
} deriving (Show,Generic)

instance FromJSON Tag

instance FromJSON Config

main :: IO ()
main = do
  file <- decodeFile "scrapper.yaml" :: IO (Maybe Config)
  print (scrapperItens <$> file)
  return ()
        -- let crawlingPath = "https://www.yomiuri.co.jp/news/" 
        -- response <- get crawlingPath
        -- let filteredTokens = (responseToFilteredTokens crawlingFilterTags response)
        -- let crawlingLinks = getLinksFromTokens filteredTokens
        -- mapM_ TIO.putStrLn crawlingLinks
        -- IO.putStrLn (renderTokens filteredTokens)
        -- responses <- (sequence) (get <$> (map T.unpack crawlingLinks))
        -- let scrapped = map (renderTokens . responseToFilteredTokens articlesFilterTags ) responses

 --       L.writeFile (T.unpack . head $ crawlingLinks) "fdsajk"
        --let rendered = map (renderTokens) scrapped
        --mapM_ (L.appendFile "yomiuri.html") (map LTE.encodeUtf8 scrapped)
