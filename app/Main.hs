{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Network.Wreq
import Data.Yaml
import Lib

main :: IO ()
main = do
    file <- decodeFileEither "scrapper.yaml" :: IO (Either ParseException [Config])
    let configs = unwrapConfig file
    let startingPages = startingPage <$> configs
    -- responses :: IO [Response L.ByteString]
    responses <- sequence (get <$> startingPages)
    print configs
    parsed <- sequence $ parseWithConfig <$> (zip configs responses)
    mapM_ writeWithConfig (parsed)