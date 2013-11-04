oska::[String] -> Char -> Int
oska state who depth
    | valid state = engine (weirdformat2goodformat state) who depth
    | otherwise = error "Invalid state"

valid::[String] -> Bool
valid state = True -- todo
