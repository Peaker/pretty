module Main (main) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (forM_)
import System.TimeIt (timeItT)
import Text.PrettyPrint.HughesPJ

stuff :: String -> String -> Double -> Rational -> Int -> Int -> Int -> Doc
stuff s1 s2 d1 r1 i1 i2 i3 =
    let a = nest i1 $ text s1
        b = double d1
        c = rational r1
        d = replicate i1 (text s2 <> b <> c <+> a)
        e = cat d $+$ cat d $$ (c <> b <+> a)
        f = parens e <> brackets c <> hcat d
        g = lparen <> f <> rparen
        h = text $ s2 ++ s1
        i = map rational ([1..(toRational i2)]::[Rational])
        j = punctuate comma i
        k = nest i3 h <> (nest (i1 + i3) $ sep i) $+$ g <> cat j
        l = cat $ punctuate (comma <> b <> comma) $ replicate i3 k
    in l

doc1 :: Doc
doc1 = stuff "Adsas ads" "dassdab weeaa xxxxx" 123.231321 ((-1)/5) 30 300 20

doc2 :: Doc
doc2 = stuff "aDSAS ADS asdasdsa sdsda xx" "SDAB WEEAA" 1333.212 ((-4)/5) 31 301 30

doc3 :: Doc
doc3 = stuff "ADsAs --____ aDS" "DasSdAB weEAA" 2533.21299 ((-4)/999) 39 399 60

{-
txt :: TextDetails -> String -> String
txt (Chr c)   s  = c:s
txt (Str s1)  s2 = s1 ++ s2
-}

vcathcat :: Doc
vcathcat = hcat . replicate 1000 . vcat . replicate 1000 $ text "["

main :: IO ()
main = do
    -- putStrLn "==================================================="
{-
    putStrLn "==================================================="
    putStrLn $ fullRender PageMode 1000 4 txt "" doc2
    putStrLn "==================================================="
    putStrLn $ fullRender PageMode 100 1.5 txt "" doc2
    putStrLn "==================================================="
    putStrLn $ fullRender ZigZagMode 1000 4 txt "" doc2
    putStrLn "==================================================="
    putStrLn $ fullRender LeftMode 1000 4 txt "" doc2
    putStrLn "==================================================="
    putStrLn $ fullRender OneLineMode 1000 4 txt "" doc3
    putStrLn "==================================================="
-}
    forM_
      [ ("doc1", doc1)
      , ("doc2", doc2)
      , ("doc3", doc3)
      , ("vcathcat", vcathcat)
      ] $ \(docName, doc) -> do
      (time, _) <- timeItT . evaluate . force $ render doc
      putStrLn $ docName ++ " rendering took: " ++ show time ++ " seconds"
    return ()
