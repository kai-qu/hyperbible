{-# LANGUAGE OverloadedStrings #-}
import Data.List
import Data.List.Split
import System.Random
import Debug.Trace

type Filename = String
type Verse = String
type Translation = String
type Chapter = Int
     
bible_csv, bible_txt, ecc_txt :: Filename
bible_csv = "bibles.csv"
bible_txt = "bibles.txt"
ecc_txt = "ecc.txt" -- ecclesiastes

gen :: StdGen
gen = mkStdGen 7 -- 5

bookName :: String
bookName = "Ecclesiastes"

nameLen :: Int
nameLen = length bookName

verseAndTranslations :: String -> [(Verse, [Translation])]
verseAndTranslations contents = map (\l -> (head l, filter (/= "\r") $ tail l)) 
                                $ map (splitOn "\t") 
                                $ lines contents

pickTrans :: StdGen -> (Verse, [Translation]) -> (StdGen, (Verse, Translation))
pickTrans g (verseName, translations) = 
          let n = length translations in -- redundant if len same for each
          let (i, g') = randomR (0, n-1) g in
          let translation = -- trace (show i ++ " " ++ show n ++ "\n" ++ show translations) $ 
                            translations !! i in
          (g', (verseName, translation))
          
mapWithGen :: StdGen -> (StdGen -> a -> (StdGen, b)) -> [a] -> (StdGen, [b])
mapWithGen g f [] = (g, [])
mapWithGen g f (x:xs) = let (g', r) = f g x in
                        let (g'', rs) = mapWithGen g' f xs in
                        (g'', r:rs)

-- badly written, sorry!
prettyPrint :: [(Verse, [Translation])] -> IO ()
prettyPrint s = mapM_ printL s
            where printL (v, ts) = do putStrLn v
                                      printts ts
                  printts = mapM_ printt
                  printt t = do putStrLn t

prettyPrint2 :: [(Verse, Translation)] -> IO ()
prettyPrint2 s = prettyPrint $ map (\(v,t) -> (v, [t])) s

prettyPrintChapter :: [(Chapter, String)] -> IO ()
prettyPrintChapter cts = mapM_ printct cts
                   where printct (c, t) = do putStrLn $ bookName ++ " " ++ show c ++ "\n"
                                             putStrLn t
                                             putStrLn ""

chapPar :: [(Chapter, [Translation])] -> [(Chapter, String)]
chapPar cts = map (\(c, ts) -> (c, intercalate " " ts)) cts
           
groupByChapter :: [(Verse, Translation)] -> [(Chapter, [Translation])]
groupByChapter verses = let verseChapter = map parseVerse verses in
                        let chapterGroups = groupBy sameChapter verseChapter in
                        map combineTranslations chapterGroups
                        where parseVerse (v, t) = (parseName v, t) -- "Ecclesiastes 13:19" -> (13, 19)
                              parseName n = let str = splitOn ":" $ drop nameLen n in
                                            (str !! 0, str !! 1)
                              sameChapter ((c1, v1), t1) ((c2, v2), t2) = (c1 == c2)
                              combineTranslations xs = (read (fst $ fst $ head xs) :: Int, map snd xs)

main :: IO ()
main = do putStrLn "Parsing"
          contents <- readFile "ecc.txt"
          -- Each verse translation-group is separated by a newline
          -- Each translation of a verse is separated by a tab
          -- The first word of each translation-group is the verse name
          -- TODO remove ending \r chars?
          let lns = verseAndTranslations contents
          let (header, bibLns) = (head lns, tail lns)
          let (g', lns_rnd) = mapWithGen gen pickTrans bibLns
          let chapterAndVerse = groupByChapter lns_rnd
          let chapterAndPar = chapPar chapterAndVerse 
          -- putStrLn "original"
          -- prettyPrint lns
          -- putStrLn "new"
          -- prettyPrint2 lns_rnd
          putStrLn "chapters"
          prettyPrintChapter chapterAndPar
