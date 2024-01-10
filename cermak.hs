type Result = [String]
pp :: Result -> IO ()
pp x = putStr (concat (map (++"\n") x))

crossword :: Result
crossword = ["...##....",
             ".#....##.",
             ".#.##....",
             "....#.#.#",
             "#.#...#.#",
             "#.#.#....",
             "....##.#.",
             ".##....#.",
             "....##.#."]
positions [] = []
positions (x:xs) = positionstmp (x:xs) 0
positionstmp :: [String] -> Int -> [(Int, Int)]
positionstmp [] _ = []
positionstmp (x:xs) n = [(n,a) | a<-position x 0] ++ positionstmp xs (n+1) 

position :: String -> Int -> [Int]
position [] _ = []
position (x:xs) n   | (x == '.') && (filterx (x:xs) 0 > 1) = n : position(drop (filterx (x:xs) 0) (x:xs)) (n+(filterx (x:xs) 0))
                    | x == '.' = position(drop (filterx (x:xs) 0) (x:xs)) (n+(filterx (x:xs) 0))
                    | otherwise = position(drop (filterh (x:xs) 0) (x:xs)) (n+(filterh (x:xs) 0))
filterx :: String -> Int -> Int
filterx [] n = n
filterx (x:xs) n    | x == '.' = filterx xs (n+1)
                    | otherwise = n
filterh :: String -> Int -> Int
filterh [] n = n
filterh (x:xs) n    | x == '#' = filterh xs (n+1)
                    | otherwise = n