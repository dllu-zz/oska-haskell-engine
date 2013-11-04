
--Daniel Lu 75592063 a7e7
--Brian Chau 30006118 b8z7

--Project 1 CPSC 312

-- ["wwww","---","--","---","bbbb"]


oska_a7e7::[String] -> Char -> Int -> [String]
oska_a7e7 state who depth
        | valid_a7e7 state      = goodformat2badformat_a7e7 (snd (engine_a7e7 (badformat2goodformat_a7e7 state) (conv_wb12_a7e7 who) depth))
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
-- converts internal board representation to the ugly format specified in the assignment
goodformat2badformat_a7e7::[Int] -> [String]
goodformat2badformat_a7e7 state = [getrow i | i <-[0..(nn-2)]]
        where n = length state
              nn = (round (sqrt (fromIntegral (length state))))
              getrow i = [conv_12wb_a7e7 (state!!(x+y*nn)) | x <-[0..(nn-1)], y <-[0..(nn-1)], okay_a7e7 x y nn, x+y==i+(div nn 2)]

-- checks if a position x, y is a valid Oska position in a board of the good format of width n
-- for example for a board of size 8, here the dots are false and the Ts are true.
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

-- displays a list of states in the good format
multiprint_a7e7::[[Int]] -> IO()
multiprint_a7e7 (x:[]) = print_a7e7 x
multiprint_a7e7 (x:xs) = do
        print_a7e7 x
        multiprint_a7e7 xs

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
              moveright = [a x ++ b x | x <- [0..(n-3)], canmoveright x]
                where canmoveright x = x/=((div n 2)-1) && (state!!x) == who && (state!!(x+1)) == 0
                      a x = take x state -- we swap the x and x+1 position
                      b x = 0:who:(drop (x+2) state)
              movedown = [a x ++ b x ++ c x | x <- [0..(n-nn-2)], canmovedown x]
                where canmovedown x = x/=(n-1-(div nn 2)) && (state!!x) == who && (state!!(x+nn)) == 0
                      a x = take x state -- we swap the x and x+n position
                      b x = 0:(take (nn-1) (drop (x+1) state))
                      c x = who:(drop (x+nn+1) state)
              eatright = [a x ++ b x | x<- [0..(n-4)], caneatright x]
                where caneatright x = x/=((div n 2)-1) && (state!!x) == who && (state!!(x+1)) == (3-who) && (state!!(x+2)) == 0
                      a x = take x state -- we swap the x and x+2 position and set position x+1 to 0
                      b x = 0:0:who:(drop (x+3) state)
              eatdown = [a x ++ b x ++ c x ++ d x | x <- [0..(n-2*nn-2)], caneatdown x]
                where caneatdown x = x/=(n-1-(div nn 2)) && (state!!x) == who && (state!!(x+nn)) == (3-who) && (state!!(x+2*nn)) == 0
                      a x = take x state -- we swap the x and x+2*nn position and set position x+nn to 0
                      b x = 0:(take (nn-1) (drop (x+1) state))
                      c x = 0:(take (nn-1) (drop (x+nn+1) state))
                      d x = who:(drop (x+2*nn+1) state)

-- Eval
-- Given a board in the good format and which colour of the pieces to move,
-- returns an integer that is the heuristic value of the board
eval_a7e7::[Int]->Int
eval_a7e7 state
  | win_a7e7 state 1 = 2000000
  | win_a7e7 state 2 = -2000000
  | otherwise = 0 -- todo

win_a7e7::[Int]->Int->Bool
win_a7e7 state who = friendlypieces/=0 && (enemypieces == 0 || friendlypieces == backrank)
        where n = length state
              nn = (round (sqrt (fromIntegral (length state))))
              enemypieces = length (filter (==(3-who)) state)
              friendlypieces = length (filter (==who) state)
              backrank 
                | who == 1 = length (filter (==who) [state!!(i + nn*(3*(div nn 2)-2-i)) | i <- [(div nn 2)-1..nn-1]])
                | who == 2 = length (filter (==who) [(reverse state)!!(i + nn*(3*(div nn 2)-2-i)) | i <- [(div nn 2)-1..nn-1]])

-- Engine
-- Given a state in the good format, which player to move, and the desired ply depth, invokes minimax engine
-- returns best move
engine_a7e7::[Int]->Int->Int->(Int,[Int])
engine_a7e7 state who 0 = (eval_a7e7 state, state)
engine_a7e7 state 1 depth 
  | win_a7e7 state 1 = (2000000, state)
  | win_a7e7 state 2 = (-2000000, state)
  | otherwise = max_a7e7 [engine_a7e7 x 2 (depth-1) | x <- movegen_a7e7 state 1]
engine_a7e7 state 2 depth 
  | win_a7e7 state 1 = (2000000, state)
  | win_a7e7 state 2 = (-2000000, state)
  | otherwise = min_a7e7 [engine_a7e7 x 1 (depth-1) | x <- map reverse (movegen_a7e7 (reverse state) 2)]

max_a7e7::[(Int,[Int])]->(Int,[Int])
max_a7e7 (x:[]) = x
max_a7e7 (x:xs) = max x (max_a7e7 xs)

min_a7e7::[(Int,[Int])]->(Int,[Int])
min_a7e7 (x:[]) = x
min_a7e7 (x:xs) = min x (min_a7e7 xs)
