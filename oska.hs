
--Daniel Lu 75592063 a7e7
--Brian Chau 30006118 b8z7

--Project 1 CPSC 312

-- ["wwww","---","--","---","bbbb"]
-- ["bbbb","---","--","---","wwww"]
-- ["wwwww","----","---","--","---","----","bbbbb"]
-- ["wwwwww","-----","----","---","--","---","----","-----","bbbbbb"]
-- ["ww--","--w","-w","-b-","b-bb"]
-- ["-------","------","-----","----","---","--","---","----","-w---","-b----","b-bbbbb"]

module Main where

main = do
  let start = ["wwwww","----","---","--","---","----","bbbbb"]
  let d = 7;
  testing start d

testing :: [String] -> Int -> IO()
testing start d = do
  --let a = oska_a7e7 start 'w' (d-3)
  let a = oska_ab_a7e7 start 'w' d
  putStr "W  "
  --print (minimax_a7e7 (badformat2goodformat_a7e7 start) 1 (d-3))
  print (alphabeta_a7e7 (badformat2goodformat_a7e7 start) 1 (-3000000) 3000000 d)
  print_b_a7e7 a
  --let b = oska_ab_a7e7 a 'b' d
  let b = oska_a7e7 a 'b' (d-3)
  putStr "B  "
  print (minimax_a7e7 (badformat2goodformat_a7e7 a) 2 (d-3))
  --print (alphabeta_a7e7 (badformat2goodformat_a7e7 a) 2 (-3000000) 3000000 d)
  print_b_a7e7 b
  if (not (win_a7e7 (badformat2goodformat_a7e7 b) 1)) && (not (win_a7e7 (badformat2goodformat_a7e7 b) 2)) then testing b d else putStrLn "done"

oska_a7e7::[String] -> Char -> Int -> [String]
oska_a7e7 state who depth
        | valid_a7e7 state      = goodformat2badformat_a7e7 (engine_wrapper_a7e7 (badformat2goodformat_a7e7 state) (conv_wb12_a7e7 who) depth)
        | otherwise             = error "Invalid state"

oska_ab_a7e7::[String] -> Char -> Int -> [String]
oska_ab_a7e7 state who depth
        | valid_a7e7 state      = goodformat2badformat_a7e7 (engine_wrapper_ab_a7e7 (badformat2goodformat_a7e7 state) (conv_wb12_a7e7 who) depth)
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
              nn = (round (sqrt (fromIntegral n)))
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

-- displays a state in the bad format
print_b_a7e7::[String] -> IO()
print_b_a7e7 state = disp_b_a7e7 state (length state)

-- displays a state in the bad format, given the length of the board
disp_b_a7e7::[String] -> Int -> IO()
disp_b_a7e7 [] n = putStrLn " "
disp_b_a7e7 (x:xs) n = do
        putStr [' ' | i <- [1..n - (length x)]]
        putStrLn (concat [c:' ':[] | c <- x])
        disp_b_a7e7 xs n

-- Move generator wrapper
-- If there are no legal moves, outputs original position
movegen_a7e7::[Int]->Int->[[Int]]
movegen_a7e7 state who = if not (null newmoves) then newmoves else [state]
        where newmoves = newmoves_a7e7 state who

-- Move generator
-- Given a board in the good format and which colour of pieces to move,
-- returns a list of boards that are reachable in one ply by the specified player
-- assumes board is rotated such that all pieces only move right and down
newmoves_a7e7::[Int]->Int->[[Int]]
newmoves_a7e7 state who = eatright ++ eatdown  ++ moveright ++ movedown
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
                where caneatright x = x/=((div n 2)-1) && ((mod (x+2) nn) == 2 + (mod x nn)) && (state!!x) == who && (state!!(x+1)) == (3-who) && (state!!(x+2)) == 0
                      a x = take x state -- we swap the x and x+2 position and set position x+1 to 0
                      b x = 0:0:who:(drop (x+3) state)
              eatdown = [a x ++ b x ++ c x ++ d x | x <- [0..(n-2*nn-2)], caneatdown x]
                where caneatdown x = x/=(n-1-(div nn 2)) && (state!!x) == who && (state!!(x+nn)) == (3-who) && (state!!(x+2*nn)) == 0
                      a x = take x state -- we swap the x and x+2*nn position and set position x+nn to 0
                      b x = 0:(take (nn-1) (drop (x+1) state))
                      c x = 0:(take (nn-1) (drop (x+nn+1) state))
                      d x = who:(drop (x+2*nn+1) state)

mobility_a7e7::[Int]->Int->Int
mobility_a7e7 state who = length (eatright ++ eatdown  ++ moveright ++ movedown)
        where n = length state
              nn = (round (sqrt (fromIntegral (length state))))
              moveright = [1 | x <- [0..(n-3)], canmoveright x]
                where canmoveright x = x/=((div n 2)-1) && (state!!x) == who && (state!!(x+1)) == 0
              movedown = [1 | x <- [0..(n-nn-2)], canmovedown x]
                where canmovedown x = x/=(n-1-(div nn 2)) && (state!!x) == who && (state!!(x+nn)) == 0
              eatright = [1 | x<- [0..(n-4)], caneatright x]
                where caneatright x = x/=((div n 2)-1) && ((mod (x+2) nn) == 2 + (mod x nn)) && (state!!x) == who && (state!!(x+1)) == (3-who) && (state!!(x+2)) == 0
              eatdown = [1 | x <- [0..(n-2*nn-2)], caneatdown x]
                where caneatdown x = x/=(n-1-(div nn 2)) && (state!!x) == who && (state!!(x+nn)) == (3-who) && (state!!(x+2*nn)) == 0

-- Eval
-- Given a board in the good format and which colour of the pieces to move,
-- returns an integer that is the heuristic value of the board
eval_a7e7::[Int]->Int
eval_a7e7 state
  | (win_a7e7 state 1) && (win_a7e7 state 2) = 0
  | win_a7e7 state 1 = 2000000
  | win_a7e7 state 2 = -2000000
  | otherwise = advanceness state 1 - advanceness (reverse state) 2 + mobility 1 - mobility 2 - popcnt 1 + popcnt 2
        where n = length state
              nn = (round (sqrt (fromIntegral n)))
              advanceness etat who = sum [x+y-(div nn 2) | x<-[0..(nn-1)], y<-[0..(nn-1)], etat!!(x+nn*y)==who]
              mobility who = mobility_a7e7 state who
              popcnt who = 3*(length (filter (==who) state))

-- Check win condition
win_a7e7::[Int]->Int->Bool
win_a7e7 state who = friendlypieces/=0 && (enemypieces == 0 || friendlypieces == backrank)
        where n = length state
              nn = (round (sqrt (fromIntegral n)))
              enemypieces = length (filter (==(3-who)) state)
              friendlypieces = length (filter (==who) state)
              backrank 
                | who == 1 = length (filter (==who) [state!!(i + nn*(3*(div nn 2)-2-i)) | i <- [(div nn 2)-1..nn-1]])
                | who == 2 = length (filter (==who) [(reverse state)!!(i + nn*(3*(div nn 2)-2-i)) | i <- [(div nn 2)-1..nn-1]])

engine_wrapper_a7e7::[Int]->Int->Int->[Int]
engine_wrapper_a7e7 state 1 depth = snd (max_a7e7 [(minimax_a7e7 x 2 depth, x) | x <- movegen_a7e7 state 1])
engine_wrapper_a7e7 state 2 depth = snd (min_a7e7 [(minimax_a7e7 x 1 depth, x) | x <- map reverse (movegen_a7e7 (reverse state) 2)])

engine_wrapper_ab_a7e7::[Int]->Int->Int->[Int]
engine_wrapper_ab_a7e7 state 1 depth = snd (max_a7e7 [(alphabeta_a7e7 x 2 (-3000000) 3000000 depth, x) | x <- movegen_a7e7 state 1])
engine_wrapper_ab_a7e7 state 2 depth = snd (min_a7e7 [(alphabeta_a7e7 x 1 (-3000000) 3000000 depth, x) | x <- map reverse (movegen_a7e7 (reverse state) 2)])

-- Minimax engine
-- Given a state in the good format, which player to move, and the desired ply depth, invokes minimax engine
-- returns heuristic value of best move
minimax_a7e7::[Int]->Int->Int->Int
minimax_a7e7 state who 0 = eval_a7e7 state
minimax_a7e7 state who depth 
  | (win_a7e7 state 1) && (win_a7e7 state 2) = 0
  | win_a7e7 state 1 = 2000000
  | win_a7e7 state 2 = -2000000
  | otherwise = maxormin [minimax_a7e7 x (3-who) (depth-1) | x <- newmoves]
          where maxormin
                  | who == 1 = max_a7e7
                  | who == 2 = min_a7e7
                newmoves
                  | who == 1 = movegen_a7e7 state 1
                  | who == 2 = map reverse (movegen_a7e7 (reverse state) 2)

-- Alpha-beta engine
-- Given a state in the good format, which player to move, and the desired ply depth, invokes alpha-beta engine
-- returns heuristic value of best move
alphabeta_a7e7::[Int]->Int->Int->Int->Int->Int
alphabeta_a7e7 state who a b 0 = eval_a7e7 state
alphabeta_a7e7 state who a b depth
  | (win_a7e7 state 1) && (win_a7e7 state 2) = 0
  | win_a7e7 state 1 = 2000000
  | win_a7e7 state 2 = -2000000
  | otherwise = alphabeta_helper_a7e7 newmoves who a b depth
          where newmoves
                  | who == 1 = movegen_a7e7 state 1
                  | who == 2 = map reverse (movegen_a7e7 (reverse state) 2)
-- helper function for alpha-beta engine
alphabeta_helper_a7e7::[[Int]]->Int->Int->Int->Int->Int
alphabeta_helper_a7e7 [] 1 a b depth = a
alphabeta_helper_a7e7 [] 2 a b depth = b
alphabeta_helper_a7e7 (x:xs) 1 a b depth = if b <= aa then aa else (alphabeta_helper_a7e7 xs 1 aa b depth)
          where aa = max a search
                search = (alphabeta_a7e7 x 2 a b (depth-1))
alphabeta_helper_a7e7 (x:xs) 2 a b depth = if bb <= a then bb else (alphabeta_helper_a7e7 xs 2 a bb depth)
          where bb = min b search
                search = (alphabeta_a7e7 x 1 a b (depth-1))

-- finds maximum of a list
max_a7e7::Ord a=>[a]->a
max_a7e7 (x:[]) = x
max_a7e7 (x:xs) = max x (max_a7e7 xs)

-- finds minimum of a list
min_a7e7::Ord a=>[a]->a
min_a7e7 (x:[]) = x
min_a7e7 (x:xs) = min x (min_a7e7 xs)

