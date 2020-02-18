module Hscrapper 
    ( 
      unwrapConfig,
      Config(..),
      parseWithConfig,
      writeWithConfig
    ) where

import System.Directory (doesFileExist)
import GHC.Generics
import Network.HTTP.Client (HttpException (..))
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
  hookItens :: [Tag],
  contentItens  :: [Tag],
  outputFilename :: String 
} deriving (Show,Generic)

instance FromJSON Attribute 
instance FromJSON Tag 
instance FromJSON Config where
  parseJSON (Object v) = Config 
      <$> v .: "domain"
      <*> v .: "startingPage"
      <*> v .:? "hookItens" .!= []
      <*> v .:? "contentItens"  .!= []
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


writeWithConfig :: String -> ([Token],Config) ->  IO ()
writeWithConfig outputDir (tokens,config) = do 
  fileExists <- doesFileExist filePath 
  if fileExists then L.appendFile (filePath) (LTE.encodeUtf8 $ renderTokens tokens) 
  else L.writeFile (filePath) (LTE.encodeUtf8 $ renderTokens tokens) where
  filePath = outputDir ++ "/" ++ outputFilename config


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
   responses = sequence ((getWith noExceptions) <$> (T.unpack <$> links))  :: IO [Response L.ByteString]
   links = map (correctDomain (T.pack domain)) (getLinksFromTokens filteredTokens)
   filteredTokens = (responseToFilteredTokens filterTags response)
   filterTags = tagToTagnameAttrTuple <$> scrap

noExceptions :: Options
noExceptions = defaults & checkResponse .~ (Just $ \_ _ -> return ())
