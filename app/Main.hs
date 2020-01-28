module Main where

--import Network.HTTP.Conduit
import Network.Wreq
import Control.Lens
import Text.HTML.Parser
import qualified Data.ByteString.Lazy as L
--import qualified Data.ByteString.Char8 as LC
--import qualified Data.ByteString.Internal as BS (c2w,w2c)
import qualified Data.Text.Lazy.Encoding as LTE (decodeUtf8)
import qualified Data.Text.Lazy.IO as IO (putStrLn,writeFile)
import qualified Data.Text as T 
import qualified Data.Text.Lazy as LT (append,pack,unpack)
import Data.Char (chr)

--openURL :: String -> IO String
--openURL x = (map BS.w2c) .  L.unpack <$> simpleHttp x

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
        let parsedHtml = LT.append  (renderTokens article) (LT.pack "</article>")
                    where 
                      article = takeWhile((/=) endToken) dropped
                      dropped = dropWhile ((/=) beginToken) tokens 
                      beginToken = TagOpen tagName [Attr (T.pack "class") (T.pack "article-content")]
                      endToken = TagClose tagName
                      tagName = T.pack "article" :: TagName
        let unpacked = LT.unpack $ parsedHtml 
        IO.writeFile "article3.html" parsedHtml
        putStrLn unpacked
        --writeFile "article2.html" article
        --let links = map f $ sections (~== "<A>") $
        --            takeWhile (~/= "<a name=haskell>") $
        --            drop 5 $ dropWhile (~/= "<a name=current>") tags
        --mapM_ putStrLn links
        --putStr $ unlines links

main :: IO ()
main = spjPapers