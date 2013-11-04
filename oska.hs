
--Daniel Lu 75592063 a7e7
--Brian Chau 30006118 b8z7

--Project 1 CPSC 312

-- ["wwww","---","--","---","bbbb"]


oska_a7e7::[String] -> Char -> Int -> [String]
oska_a7e7 state who depth
        | valid_a7e7 state      = []-- goodformat2badformat_a7e7 (engine_a7e7 (badformat2goodformat_a7e7 state) (conv_wb12_a7e7 who) depth)
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

-- converts the ugly format specified in the assignment to the internal board representation
-- the internal board representation is an array of integers of length n^2 and is rotated by 45 degrees
-- such that white pieces can only move down and right
-- white pieces are changed to 1
-- black pieces are changed to 2
badformat2goodformat_a7e7::[String] -> [Int]
badformat2goodformat_a7e7 state = [getpiece x y | y <-[0..(n-1)], x <-[0..(n-1)]]
        where n = (length (state!!0))*2 - 2
              getpiece x y = if okay_a7e7 x y n then conv_wb12_a7e7 (state!!(getx x y)!!(gety x y)) else -1
                where getx x y = x+y-(div n 2)
                      gety x y = max (x - (div n 2) + 1) ((div n 2) - y)

-- checks if a position x, y is a valid Oska position in a board of the good format of width n
-- for example for a a board of size 8, here the dots are false and the Ts are true.
-- ....T...
-- ...TT...
-- ..TTT...
-- .TTTTTTT
-- TTTTTTT.
-- ...TTT..
-- ...TT...
-- ...T....
okay_a7e7::Int -> Int -> Int -> Bool
okay_a7e7 x y n = triangles && squares
        where triangles = (x+y) >= (div n 2) && (x+y) <= (2*n - 2 - (div n 2))
              squares = not (x<=a && y>=b) && not (x>=b && y<=a)
                where a = (div n 2) - 2
                      b = (div n 2) + 1

-- these functions are self-explanatory
conv_wb12_a7e7::Char -> Int
conv_wb12_a7e7 'w' = 1
conv_wb12_a7e7 'b' = 2
conv_wb12_a7e7 '-' = 0

conv_12wb_a7e7::Int -> Char
conv_12wb_a7e7 1 = 'w'
conv_12wb_a7e7 2 = 'b'
conv_12wb_a7e7 0 = '-'
conv_12wb_a7e7 _ = ' '

-- displays a state in the good format
print_a7e7::[Int] -> IO()
print_a7e7 state = disp_a7e7 state (round (sqrt (fromIntegral (length state))))

-- displays a state in the good format, given the width of the board
disp_a7e7::[Int] -> Int -> IO()
disp_a7e7 [] n = putStrLn " "
disp_a7e7 state n = do
        putStrLn [(conv_12wb_a7e7 (state!!i)) | i <- [0..(n-1)]]
        disp_a7e7 (drop n state) n

-- Move generator
-- Given a board in the good format and which colour of pieces to move,
-- returns a list of boards that are reachable in one ply by the specified player
-- assumes board is rotated such that all pieces only move right and down
movegen_a7e7::[Int]->Int->[[Int]]
movegen_a7e7 state who = moveright ++ eatright ++ movedown ++ eatdown 
        where n = length state
              nn = (round (sqrt (fromIntegral (length state))))
              moveright = [take x state ++ (0:who:(drop (x+2) state)) | x <- [0..(n-3)], canmoveright x]
                where canmoveright x = (state!!x) == who && (state!!(x+1)) == 0
              movedown = [take x state ++ 0:(take (nn-1) (drop (x+1) state)) ++ who:(drop (x+nn+1) state) | x <- [0..(n-nn-2)], canmovedown x]
                where canmovedown x = (state!!x) == who && (state!!(x+nn)) == 0
              eatright = [take x state ++ (0:0:who:(drop (x+3) state)) | x<- [0..(n-4)], caneatright x]
                where caneatright x = (state!!x) == who && (state!!(x+1)) == (3-who) && (state!!(x+2)) == 0
              eatdown = [take x state ++ 0:(take (nn-1) (drop (x+1) state)) ++ 0:(take (nn-1) (drop (x+nn+1) state)) ++ who:(drop (x+2*nn+1) state) | x <- [0..(n-2*nn-2)], caneatdown x]
                where caneatdown x = (state!!x) == who && (state!!(x+nn)) == (3-who) && (state!!(x+2*nn)) == 0

-- Eval
-- Given a board in the good format and which colour of the pieces to move,
-- returns an integer that is the heuristic value of the board
eval_a7e7::[Int]->Int->Int
eval_a7e7 state who = 0 -- todo

-- Engine
-- Given a state in the good format, which player to move, and the desired ply depth, invokes minimax engine
-- returns best move
engine_a7e7::[Int]->Int->[Int]
engine_a7e7 state who = state -- todo

