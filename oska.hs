
--Daniel Lu 75592063 a7e7
--Brian Chau 30006118 b8z7

--Project 1 CPSC 312

-- ["wwww","---","--","---","bbbb"]


oska_a7e7::[String] -> Char -> Int -> [String]
oska_a7e7 state who depth
        | valid_a7e7 state      = []-- goodformat2badformat_a7e7 (engine_a7e7 (badformat2goodformat_a7e7 state) who depth)
        | otherwise             = error "Invalid state"

-- a valid state must be a list of strings of valid lengths
-- furthermore, each string must only be made of characters '-', 'w', and 'b'
valid_a7e7::[String] -> Bool
valid_a7e7 state = checklengths state && checkcontents state
        where n = length state
              checklengths x = odd n && n >=4 && checkeachlength x
                where checkeachlength x = sum [abs (length (x!!i) - (expectedstringlength i))|i<-[0..(n-1)]] == 0
                        where expectedstringlength i = 2 + abs (i - (div n 2))
              checkcontents x = sum [abs (length i - countchar i 'w' - countchar i 'b' - countchar i '-')|i<-x] == 0
                        where countchar s c = length (filter (==c) s)

badformat2goodformat_a7e7::[String] -> [Int]
badformat2goodformat_a7e7 state = [getpiece x y | y <-[0..(n-1)], x <-[0..(n-1)]]
        where n = (length (state!!0))*2 - 2
              getpiece x y = if okay_a7e7 x y n then conv_wb12_a7e7 (state!!(getx x y)!!(gety x y)) else -1
                where getx x y = x+y-(div n 2)
                      gety x y = max (x - (div n 2) + 1) ((div n 2) - y)

okay_a7e7::Int -> Int -> Int -> Bool
okay_a7e7 x y n = triangles && squares
        where triangles = (x+y) >= (div n 2) && (x+y) <= (2*n - 2 - (div n 2))
              squares = not (x<=a && y>=b) && not (x>=b && y<=a)
                where a = (div n 2) - 2
                      b = (div n 2) + 1

conv_wb12_a7e7::Char -> Int
conv_wb12_a7e7 'w' = 1
conv_wb12_a7e7 'b' = 2
conv_wb12_a7e7 '-' = 0

conv_12wb_a7e7::Int -> Char
conv_12wb_a7e7 1 = 'w'
conv_12wb_a7e7 2 = 'b'
conv_12wb_a7e7 0 = '-'
conv_12wb_a7e7 _ = ' '

print_a7e7::[Int] -> IO()
print_a7e7 state = disp_a7e7 state (round (sqrt (fromIntegral (length state))))

disp_a7e7::[Int] -> Int -> IO()
disp_a7e7 [] n = putStrLn " "
disp_a7e7 state n = do
        putStrLn [(conv_12wb_a7e7 (state!!i)) | i <- [0..(n-1)]]
        disp_a7e7 (drop n state) n

movegen_a7e7::[Int]->[[Int]]
movegen_a7e7 state = [state] --todo
