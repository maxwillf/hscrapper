{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Network.Wreq hiding (header)
import Data.Yaml hiding (Parser)
import Lib
import Options.Applicative 
import Data.Semigroup ((<>))

data Arguments = Arguments {
  inputFile :: String,
  outputDir :: String
}

argsParser :: Parser Arguments
argsParser = Arguments 
          <$> strOption 
                (long "inputFile"
                <> short 'i'
                <> metavar "InputConfigFile"
                <> help "Input Config file needed for scrapping"
                )
          <*> strOption
                (long "outputDir"
                <> short 'o'
                <> metavar "OutputDirectory"
                <> help "Output Directory"
                )
opts :: ParserInfo Arguments
opts = info (argsParser <**> helper)
          (fullDesc
          <> progDesc "Utility for casual website scrapping"
          <> header "Haskell scrapper")

main :: IO ()
main = do
    args <- execParser opts
    file <- decodeFileEither (inputFile args) :: IO (Either ParseException [Config])
    let configs = unwrapConfig file
    let startingPages = startingPage <$> configs
    -- responses :: IO [Response L.ByteString]
    responses <- sequence (get <$> startingPages)
    print configs
    parsed <- sequence $ parseWithConfig <$> (zip configs responses)
    mapM_ (writeWithConfig $ outputDir args) (parsed)