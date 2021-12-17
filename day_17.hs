module Main where

type Position = (Int, Int)
type Velocity = (Int, Int)
type State = (Position, Velocity)
type Target = (Position, Position)

step :: State -> State
step ((x, y), (vx, vy))
    | vx == 0 = ((x+vx, y+vy), (vx    , vy - 1))
    | vx < 0  = ((x+vx, y+vy), (vx + 1, vy - 1))
    | vx > 0  = ((x+vx, y+vy), (vx - 1, vy - 1))

isWithingTarget :: State -> Target -> Bool
isWithingTarget ((x,y), _) ((xmin, xmax), (ymin, ymax)) = x >= xmin && x <= xmax && y >= ymin && y <= ymax  

isStillValid :: State -> Target -> Bool
isStillValid ((x,y), _) (_, (ymin, ymax)) = y >= ymin

isValid :: State -> Target -> Bool
isValid s t = do
    if not $ isStillValid s t then False else if isWithingTarget s t then True else isValid (step s) t

findMaxY :: State -> Target -> Int
findMaxY ((x, y), v) t = do
    let s2 = step ((x, y), v)
    if (snd $ fst s2) < y then y else findMaxY s2 t

bruteForce :: Target -> [State]
bruteForce t = do
    let brute = [((0,0), (x, y)) | x <- [-300..300], y <- [-300..300]]
    filter (\e -> isValid e t) brute

main :: IO()
main = do
    let target = ((241, 275), (-75, -49))
    let targets =  bruteForce target
    print $ maximum $ map (\e -> findMaxY e target) targets
    print $ length $ targets
