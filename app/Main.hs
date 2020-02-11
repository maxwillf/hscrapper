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
import Data.Bitraversable  as BI (bitraverse)

import Data.Yaml
import Data.Aeson.Types (prependFailure,typeMismatch)


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


filterTokens :: [(TagName,[Attr])] -> [Token] -> [Token]
filterTokens [] tokens = tokens
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
responseToFilteredTokens [] response  = responseToTokens response
responseToFilteredTokens filterTags response  = 
                  filterTokens filterTags (responseToTokens response)

------------------------------------- yaml parsing variables

data Attribute = Attribute {
  attrName :: String,
  attrVal :: String
} deriving (Show,Generic)

data Tag = Tag {
  tag  :: String,
  attr :: [Attribute]
} deriving (Show,Generic)

data Config = Config {
  domain :: String,
  startingPage :: String,
  scrapperItens :: [Tag],
  articleItens  :: [Tag],
  outputFilename :: String 
} deriving (Show,Generic)

instance FromJSON Attribute 
instance FromJSON Tag 

instance FromJSON Config where
  parseJSON (Object v) = Config 
      <$> v .: "domain"
      <*> v .: "startingPage"
      <*> v .:? "scrapperItens" .!= []
      <*> v .:? "articleItens"  .!= []
      <*> v .: "outputFilename" 
  parseJSON invalid = prependFailure "parsing Config failed, " 
                      (typeMismatch "Object" invalid)

unwrapConfig :: Either ParseException [Config] -> [Config]
unwrapConfig (Left x) = []
unwrapConfig (Right x) = x

tagToTagnameAttrTuple :: Tag -> (TagName,[Attr])
tagToTagnameAttrTuple tagVar = (tagName, attributes) where
                               tagName = T.pack $ tag tagVar 
                               attrToAttr = (\(Attribute name val) -> Attr (T.pack  name) (T.pack val))
                               attributes = [ attrToAttr attribute | attribute <- (attr tagVar)]


writeWithConfig :: ([Token],Config) ->  IO ()
writeWithConfig (tokens,config) = L.appendFile (outputFilename config) (LTE.encodeUtf8 $ renderTokens tokens)


correctDomain :: T.Text -> T.Text -> T.Text 
correctDomain domain url 
  | T.isPrefixOf "http" url = url
correctDomain domain url = T.append domain url
-- requests all scrapped anchor links if there are any scrapItens and returns the same page if there aren't
parseWithConfig :: (Config,Response L.ByteString) ->  IO ([Token],Config)
parseWithConfig (Config domain spage [] articleFilters outputFilename,response) =
   return (responseToFilteredTokens (tagToTagnameAttrTuple <$> articleFilters) response,Config domain spage [] articleFilters outputFilename)

parseWithConfig (Config domain spage scrap articleFilters outputFilename,response) = 
   BI.bitraverse id pure (filteredResponses,Config domain spage scrap articleFilters outputFilename) where
   filteredResponses = (concatMap (responseToFilteredTokens tagTuples)) <$> responses
   tagTuples = tagToTagnameAttrTuple <$> articleFilters
   responses = sequence (get <$> (T.unpack <$> links))  :: IO [Response L.ByteString]
   links = map (correctDomain (T.pack domain)) (getLinksFromTokens filteredTokens)
   filteredTokens = (responseToFilteredTokens filterTags response)
   filterTags = tagToTagnameAttrTuple <$> scrap

main :: IO ()
main = do
  file <- decodeFileEither "scrapper.yaml" :: IO (Either ParseException [Config])
  let configs = unwrapConfig file
  let startingPages = startingPage <$> configs
  -- responses :: IO [Response L.ByteString]
  let responses = sequence(get <$> startingPages)
  resps <- responses
  -- let comprehension = [() <- configs]
  print configs
  parsed <- sequence $ parseWithConfig <$> (zip configs resps)
  mapM_ writeWithConfig (parsed)
  putStrLn ""
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

-- articlesFilterTags :: [(TagName,[Attr])]
-- articlesFilterTags = [
--   ("div", [Attr "class" "article-header"]),
--   ("div", [Attr "class" "p-main-contents"])]

-- crawlingFilterTags :: [(TagName,[Attr])]  -- | tagname == "a" = domain ++ hrefValue where
-- crawlingFilterTags = [  --                           hrefValue = (\(Attr x y) -> y) <$> hrefAttr
--   -- ("h3" , [Attr "class" "c-list-title"])  --                           hrefAttr = (List.find (\(Attr y z) -> y == "href") attrs)
--     ("li", [Attr "class" "p-list-item  "]),
--     ("li", [Attr "class" "p-list-item p-list-item--small-thumb "])
--    ]