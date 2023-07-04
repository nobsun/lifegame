module Main where

import Data.List

main :: IO ()
main = life univ0

drive :: ([String] -> [String]) -> (String -> String)
drive f = unlines . f . lines

life :: Univ -> IO ()
life = interact . drive . game

game :: Univ -> ([String] -> [String])
game u = map (showUniv . univ) . transaction . initGame u

initGame :: Univ -> [String] -> Game
initGame u iis = (iis, u)

transaction :: Game -> [Game]
transaction g = g : rests
    where
        rests
            | gameOver g = []
            | otherwise  = transaction (step g)

gameOver :: Game -> Bool
gameOver = null . inputs

step :: Game -> Game
step (i:is, u) = (is, evolution u)

evolution :: Univ -> Univ
evolution u = survivors u ++ births u

survivors :: Univ -> Univ
survivors u = [ c | c <- u
                  , liveNeighbs u c `elem` [2,3]]

births :: Univ -> Univ
births u = [ c | c <- nub $ concatMap neighbs u
               , notAlive u c
               , liveNeighbs u c == 3 ]

liveNeighbs :: Univ -> Cell -> Int
liveNeighbs u = length . filter (alive u) . neighbs

neighbs :: Cell -> [Cell]
neighbs (i,j) = [(w,n),(w,j),(w,s),(i,n),(i,s),(e,n),(e,j),(e,s)]
    where
        w = pred i `mod` width
        e = succ i `mod` width
        n = pred j `mod` height
        s = succ j `mod` height

alive :: Univ -> Cell -> Bool
alive = flip elem

notAlive :: Univ -> Cell -> Bool
notAlive u = not . alive u

type Game = ([String], Univ)
univ :: Game -> Univ
univ = snd

inputs :: Game -> [String]
inputs = fst

type Univ = [Cell]
type Cell = Pos
type Pos  = (Int, Int)
type Dim  = (Int, Int)

dimension :: Dim
dimension = (width, height)
width, height :: Int
width  = 32
height = 32

showUniv :: Univ -> String
showUniv = (cls ++) . concatMap showCell

showCell :: Cell -> String
showCell c = goto c ++ "#"

goto :: Cell -> String
goto (i,j) = "\ESC[" ++ show (succ j) ++ ";" ++ show (succ i) ++ "H"

cls :: String
cls = "\ESC[2J" 

univ0 :: Univ
univ0 = pentadecathlon

pentadecathlon :: Univ
pentadecathlon = translate (11, 16)
    $ genUniv
    [ "##########" ]

rpentomino :: Univ
rpentomino = translate (16, 16) 
    $ genUniv
    ["-##"
    ,"##-"
    ,"-#-"]

translate :: Pos -> Univ -> Univ
translate p = map (add p)

add :: Pos -> Cell -> Cell
add (a, b) (i, j) = (a + i, b + j)

genUniv :: [String] -> Univ
genUniv css = foldr phi [] $ zip (range (w, h)) (concat css)
    where
        phi (c, '#') cs = c:cs
        phi _        cs = cs
        w = length $ head css
        h = length css

range :: Dim -> [Cell]
range (w, h) = [ (i, j) | j <- [0 .. pred h]
                        , i <- [0 .. pred w]]
