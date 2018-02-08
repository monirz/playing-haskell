-- Quick sort implementation
qsort :: [Int] -> [Int]

qsort [] = []
qsort (p:t) =
    let smaller = qsort [x | x <- t, x < p ]
        larger  = qsort [x | x <- t, x >= p]
    in  smaller ++ [p] ++  larger
