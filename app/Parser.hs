module Main where

parseArticle :: LC.ByteString -> BC.ByteString
parseArticle jsonBody = articleWithEndingTag 
       where
         articleWithEndingTag = BC.append article "</article>"
         article = fst $ BC.breakSubstring "</article>" firstPart
         firstPart = snd $ BC.breakSubstring "<article" strictBody 
         strictBody = LC.toStrict jsonBody
       
       --BC.putStrLn broken
       --BC.writeFile "article.html" broken