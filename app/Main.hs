module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Network.HTTP.Simple
import Data.Text
import Data.Text.Internal.Search
import Data.List (findIndex)

redditDomain :: BC.ByteString
redditDomain = "www.reddit.com"

path :: BC.ByteString
path = "/r/LegionFX"


japanHost = "www.japantimes.co.jp"
japanPath = "/news/2020/01/22/national/politics-diplomacy/opposition-shinzo-abe-scandals-diet-japan/"

buildRequest token host method path   = setRequestMethod method
                                  $  setRequestHost host
                                  $  setRequestHeader "token" [token]
                                  $  setRequestPath path
                                  $  setRequestSecure True
                                  $  setRequestPort 443
                                  $  defaultRequest

request :: Request
request = buildRequest "" japanHost "GET" japanPath

main :: IO ()
main = do
       jsonBody <- LC.readFile "data.json"
       let strictBody = LC.toStrict jsonBody
       let broken = articleWithEndingTag where
                  articleWithEndingTag = BC.append article "</article>"
                  article = fst $ BC.breakSubstring "</article>" firstPart
                  firstPart = snd $ BC.breakSubstring "<article" strictBody 
       
       BC.putStrLn broken
       BC.writeFile "article.html" broken