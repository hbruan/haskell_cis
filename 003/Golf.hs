module Golf where

-- exercise 1
skip :: Int -> [a] -> [a]
skip n = map head . takeWhile (not . null) . iterate (drop n) . drop (n-1)

skips :: [a] -> [[a]]
skips [] = []
skips as = [ skip n as | n <- [1..(length as)]]

-- exercise 2
localMaxima :: [Integer] -> [Integer]
localMaxima s@(x:y:z:zs)
  | y > x && y > z = y : localMaxima (drop 1 s)
  | otherwise = localMaxima (drop 1 s)
localMaxima _ = []

-- exercise 3
bottomLine = concat (replicate 4 "=") ++ "\n123456789"

counter :: [Integer] -> [Integer]
counter [] = replicate 10 0
counter [x] = replicate (fromInteger x) 0 ++ [1] ++ replicate (9-fromInteger x) 0
counter (x:xs) = zipWith (+) (counter [x]) (counter xs)

counterHis :: [Integer] -> String
counterHis ns
  | maximum ns <= 0 = concat (replicate 10 "0")
  | maximum ns == 1 = concatMap show ns
  | otherwise = counterHis (map (\x -> max (x-1) 0) ns) ++ "\n" ++ concatMap (\x -> if x == 0 then "0" else "1") ns

histogram :: [Integer] -> String
histogram ns = counterHis (counter ns) ++ bottomLine
