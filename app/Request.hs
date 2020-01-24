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
  response <- httpLBS request
  let status = getResponseStatusCode response
  if status == 200
     then do
       print "saving request to file"
       let jsonBody = getResponseBody response
       --LC.putStrLn jsonBody
       L.writeFile "data.json" jsonBody
       let strictBody = LC.toStrict jsonBody
       let broken = BC.breakSubstring "<article" strictBody
       BC.putStrLn $ fst broken
       BC.putStrLn $ snd broken
       let joinedStr = joinedString where
                  firstString = fst broken
                  secondString = snd broken
                  joinedString = BC.append firstString secondString
       print . show $ joinedStr == strictBody
       else print "request failed with error"
