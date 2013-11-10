--Daniel Lu 75592063 a7e7
--Brian Chau 30006118 b8z7

--Project 1 CPSC 312
-- This file differs from Oska.hs in that it uses a bitboard representation.

module OskaB where
import Data.Bits
-- runs the oska engine with minimax search (slower)
oska_b8z7::[String] -> Char -> Int -> [String]
oska_b8z7 state who depth
        | valid_b8z7 state      = goodformat2badformat_b8z7 (engine_wrapper_b8z7 (badformat2goodformat_b8z7 state) (conv_wb12_b8z7 who) depth)
        | otherwise             = error "Invalid state"

-- runs the oska engine with alpha-beta search (faster)
oska_ab_b8z7::[String] -> Char -> Int -> [String]
oska_ab_b8z7 state who depth
        | valid_b8z7 state      = goodformat2badformat_b8z7 (engine_wrapper_ab_b8z7 (badformat2goodformat_b8z7 state) (conv_wb12_b8z7 who) depth)
        | otherwise             = error "Invalid state"

-- a valid state must be a list of strings of valid lengths
-- furthermore, each string must only be made of characters '-', 'w', and 'b'
valid_b8z7::[String] -> Bool
valid_b8z7 state = checklengths state && checkcontents state
        where n = length state
              checklengths x = odd n && n >=4 && checkeachlength x
                where checkeachlength x = sum [abs (length (x!!i) - (expectedstringlength i))|i<-[0..(n-1)]] == 0
                        where expectedstringlength i = 2 + abs (i - (div n 2))
              checkcontents x = sum [abs (length i - countchar i 'w' - countchar i 'b' - countchar i '-')|i<-x] == 0
                        where countchar s c = length (filter (==c) s)

-- converts the ugly format specified in the assignment to the internal board representation
-- the internal board representation a bitboard length 2n^2 and is rotated by 45 degrees
-- such that white pieces can only move down and right
-- white pieces are changed to 1
-- black pieces are changed to 2
badformat2goodformat_b8z7::[String] -> (Integer,Int)
badformat2goodformat_b8z7 state = (sum [(getpiece x y) `shiftL` (2*(x+n*y)) | y <-[0..(n-1)], x <-[0..(n-1)]], n)
        where n = (length (state!!0))*2 - 2
              getpiece x y = if okay_b8z7 x y n then conv_wb12_b8z7 (state!!(getx x y)!!(gety x y)) else 3
                where getx x y = x+y-(div n 2)
                      gety x y = max (x - (div n 2) + 1) ((div n 2) - y)
-- converts internal board representation to the ugly format specified in the assignment
goodformat2badformat_b8z7::(Integer,Int) -> [String]
goodformat2badformat_b8z7 state = [getrow i | i <-[0..(n-2)]]
        where n = snd state
              getrow i = [conv_12wb_b8z7 (getpos state x y) | x <-[0..(n-1)], y <-[0..(n-1)], okay_b8z7 x y n, x+y==i+(div n 2)]
-- gets the piece at the specified position in a bitboard state
getpos::(Integer,Int) -> Int -> Int -> Integer
getpos state x y = (bitboard .&. (3 .<<. (2*(x+y*n)))) .>>. (2*(x+y*n))
        where n = snd state
              bitboard = fst state
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
okay_b8z7::Int -> Int -> Int -> Bool
okay_b8z7 x y n = triangles && squares
        where triangles = (x+y) >= (div n 2) && (x+y) <= (2*n - 2 - (div n 2))
              squares = not (x<=a && y>=b) && not (x>=b && y<=a)
                where a = (div n 2) - 2
                      b = (div n 2) + 1

-- these functions are self-explanatory
conv_wb12_b8z7::Char -> Integer
conv_wb12_b8z7 'w' = 1
conv_wb12_b8z7 'b' = 2
conv_wb12_b8z7 '-' = 0

conv_12wb_b8z7::Integer -> Char
conv_12wb_b8z7 1 = 'w'
conv_12wb_b8z7 2 = 'b'
conv_12wb_b8z7 0 = '-'
conv_12wb_b8z7 _ = ' '

-- displays a state in the bad format
print_b_b8z7::[String] -> IO()
print_b_b8z7 state = disp_b_b8z7 state (length state)

-- displays a state in the bad format, given the length of the board
disp_b_b8z7::[String] -> Int -> IO()
disp_b_b8z7 [] n = putStrLn " "
disp_b_b8z7 (x:xs) n = do
        putStr [' ' | i <- [1..n - (length x)]]
        putStrLn (concat [c:' ':[] | c <- x])
        disp_b_b8z7 xs n

-- Move generator wrapper
-- If there are no legal moves, outputs original position
movegen_b8z7::(Integer,Int)->Integer->[(Integer,Int)]
movegen_b8z7 state who = if not (null newmoves) then newmoves else [state]
        where newmoves = newmoves_b8z7 state who

-- Move generator
-- Given a board in the good format and which colour of pieces to move,
-- returns a list of boards that are reachable in one ply by the specified player.
-- Thanks to my excellent choicoe of variable names, code should be self-explanatory :D
newmoves_b8z7::(Integer,Int)->Integer->[(Integer,Int)]
newmoves_b8z7 state who 
  | who == 1 = eatright ++ eatdown ++ movedown ++ moveright
  | who == 2 = eatleft ++ eatup ++ moveup ++ moveleft
        where n = snd state -- the size of the bitboard
              b = fst state -- the bitboard
              one = 3::Integer -- bitmask for one bitboard position (11)
              two = 15::Integer -- bitmask for two bitboard positions (1111)
              thr = 63::Integer -- bitmask for three bitboard positions (111111)
              moveright = [next x y | x<-[0..(n-2)], y<-[0..(n-1)], canmoveright x y]
                where next x y = (b `xor` ((who .<<. (2*(x+n*y)) ) .|. (who .<<. (2*(x+1+n*y)) )), n)
                      canmoveright x y = (b .&. (two .<<. (2*(x+n*y)) )) == (who .<<. (2*(x+n*y)) )
              movedown = [next x y | x<-[0..(n-1)], y<-[0..(n-2)], canmovedown x y]
                where next x y = (b `xor` ((who .<<. (2*(x+n*y)) ) .|. (who .<<. (2*(x+n+n*y)) )), n)
                      canmovedown x y = (b .&. ((one .|. (one .<<. (2*n) )) .<<. (2*(x+n*y)) )) == (who .<<. (2*(x+n*y)) )
              eatright = [next x y | x<-[0..(n-3)], y<-[0..(n-1)], caneatright x y]
                where next x y = (b `xor` ((who .<<. (2*(x+n*y)) ) .|. ((3-who) .<<. (2*(x+1+n*y)) ) .|. (who .<<. (2*(x+2+n*y)) )), n)
                      caneatright x y = (b .&. (thr .<<. (2*(x+n*y)) )) == (who .<<. (2*(x+n*y)) ) .|. ((3-who) .<<. (2*(x+1+n*y)) )
              eatdown = [next x y | x<-[0..(n-1)], y<-[0..(n-3)], caneatdown x y]
                where next x y = (b `xor` ((who .<<. (2*(x+n*y)) ) .|. ((3-who) .<<. (2*(x+n+n*y)) ) .|. (who .<<. (2*(x+2*n+n*y)) )), n)
                      caneatdown x y = (b .&. ((one .|. (one .<<. (2*n) ) .|. (one .<<. (4*n) )) .<<. (2*(x+n*y)) )) == (who .<<. (2*(x+n*y)) ) .|. ((3-who) .<<. (2*(x+n+n*y)) )
              moveleft = [next x y | x<-[0..(n-2)], y<-[0..(n-1)], canmoveleft x y]
                where next x y = (b `xor` ((who .<<. (2*(x+n*y)) ) .|. (who .<<. (2*(x+1+n*y)) )), n)
                      canmoveleft x y = (b .&. (two .<<. (2*(x+n*y)) )) == (who .<<. (2*(x+1+n*y)) )
              moveup = [next x y | x<-[0..(n-1)], y<-[0..(n-2)], canmoveup x y]
                where next x y = (b `xor` ((who .<<. (2*(x+n*y)) ) .|. (who .<<. (2*(x+n+n*y)) )), n)
                      canmoveup x y = (b .&. ((one .|. (one .<<. (2*n) )) .<<. (2*(x+n*y)) )) == (who .<<. (2*(x+n+n*y)) )
              eatleft = [next x y | x<-[0..(n-3)], y<-[0..(n-1)], caneatleft x y]
                where next x y = (b `xor` ((who .<<. (2*(x+n*y)) ) .|. ((3-who) .<<. (2*(x+1+n*y)) ) .|. (who .<<. (2*(x+2+n*y)) )), n)
                      caneatleft x y = (b .&. (thr .<<. (2*(x+n*y)) )) == (who .<<. (2*(x+2+n*y)) ) .|. ((3-who) .<<. (2*(x+1+n*y)) )
              eatup = [next x y | x<-[0..(n-1)], y<-[0..(n-3)], caneatup x y]
                where next x y = (b `xor` ((who .<<. (2*(x+n*y)) ) .|. ((3-who) .<<. (2*(x+n+n*y)) ) .|. (who .<<. (2*(x+2*n+n*y)) )), n)
                      caneatup x y = (b .&. ((one .|. (one .<<. (2*n) ) .|. (one .<<. (4*n) )) .<<. (2*(x+n*y)) )) == (who .<<. (2*(x+2*n+n*y)) ) .|. ((3-who) .<<. (2*(x+n+n*y)) )

-- the number of legal moves that can be made
mobility_b8z7::(Integer,Int)->Integer->Int
mobility_b8z7 state who = length (newmoves_b8z7 state who)

-- Eval
-- Given a board in the good format and which colour of the pieces to move,
-- returns an (integer,Int) that is the heuristic value of the board
eval_b8z7::(Integer,Int)->Int
eval_b8z7 state
  | (win_b8z7 state 1) && (win_b8z7 state 2) = 0
  | win_b8z7 state 1 = 2000000
  | win_b8z7 state 2 = -2000000
  | otherwise = advanceness 1 - advanceness 2 + mobility_b8z7 state 1 - mobility_b8z7 state 2
        where n = snd state
              advanceness 1 = sum [score x y | x<-[0..(n-1)], y<-[0..(n-1)], getpos state x y == 1]
              advanceness 2 = sum [score x y | x<-[0..(n-1)], y<-[0..(n-1)], getpos state (n-x-1) (n-y-1) == 2]
              score x y = if x+y == (div (3*n) 2) - 2 then n*n else 4*(x+y-n-(div n 2)) - abs (x-y)

-- Check win condition
win_b8z7::(Integer,Int)->Integer->Bool
win_b8z7 state who = friendlypieces/=0 && (enemypieces == 0 || friendlypieces == backrank)
        where n = snd state
              bitboard = fst state
              enemypieces = length (filter (==(3-who)) pieces)
              friendlypieces = length (filter (==who) pieces)
              pieces = [getpos state x y | x <-[0..(n-1)], y <-[0..(n-1)]]
              backrank 
                | who == 1 = length (filter (==who) [getpos state x y | x <-[0..(n-1)], y <-[0..(n-1)], x+y == (div (3*n) 2) - 2])
                | who == 2 = length (filter (==who) [getpos state x y | x <-[0..(n-1)], y <-[0..(n-1)], x+y == (div n 2)])

engine_wrapper_b8z7::(Integer,Int)->Integer->Int->(Integer,Int)
engine_wrapper_b8z7 state who depth = snd (maxormin [(minimax_b8z7 x (3-who) depth, x) | x <- movegen_b8z7 state who])
        where maxormin
                | who == 1 = max_b8z7
                | who == 2 = min_b8z7

engine_wrapper_ab_b8z7::(Integer,Int)->Integer->Int->(Integer,Int)
engine_wrapper_ab_b8z7 state who depth = snd (maxormin [(alphabeta_b8z7 x (3-who) (-3000000) 3000000 depth, x) | x <- movegen_b8z7 state who])
        where maxormin
                | who == 1 = max_b8z7
                | who == 2 = min_b8z7

-- Minimax engine
-- Given a state in the good format, which player to move, and the desired ply depth, invokes minimax engine
-- returns heuristic value of best move
minimax_b8z7::(Integer,Int)->Integer->Int->Int
minimax_b8z7 state who 0 = eval_b8z7 state
minimax_b8z7 state who depth 
  | (win_b8z7 state 1) && (win_b8z7 state 2) = 0
  | win_b8z7 state 1 = 2000000
  | win_b8z7 state 2 = -2000000
  | otherwise = maxormin [minimax_b8z7 x (3-who) (depth-1) | x <- newmoves]
          where maxormin
                  | who == 1 = max_b8z7
                  | who == 2 = min_b8z7
                newmoves = movegen_b8z7 state who

-- Alpha-beta engine
-- Given a state in the good format, which player to move, and the desired ply depth, invokes alpha-beta engine
-- returns heuristic value of best move
alphabeta_b8z7::(Integer,Int)->Integer->Int->Int->Int->Int
alphabeta_b8z7 state who a b 0 = eval_b8z7 state
alphabeta_b8z7 state who a b depth
  | (win_b8z7 state 1) && (win_b8z7 state 2) = 0
  | win_b8z7 state 1 = 2000000
  | win_b8z7 state 2 = -2000000
  | otherwise = alphabeta_helper_b8z7 newmoves who a b depth
          where newmoves = movegen_b8z7 state who
-- helper function for alpha-beta engine
alphabeta_helper_b8z7::[(Integer,Int)]->Integer->Int->Int->Int->Int
alphabeta_helper_b8z7 [] 1 a b depth = a
alphabeta_helper_b8z7 [] 2 a b depth = b
alphabeta_helper_b8z7 (x:xs) 1 a b depth = if b <= aa then aa else (alphabeta_helper_b8z7 xs 1 aa b depth)
          where aa = max a search
                search = (alphabeta_b8z7 x 2 a b (depth-1))
alphabeta_helper_b8z7 (x:xs) 2 a b depth = if bb <= a then bb else (alphabeta_helper_b8z7 xs 2 a bb depth)
          where bb = min b search
                search = (alphabeta_b8z7 x 1 a b (depth-1))

-- finds maximum of a list
max_b8z7::Ord a=>[a]->a
max_b8z7 (x:[]) = x
max_b8z7 (x:xs) = max x (max_b8z7 xs)

-- finds minimum of a list
min_b8z7::Ord a=>[a]->a
min_b8z7 (x:[]) = x
min_b8z7 (x:xs) = min x (min_b8z7 xs)

-- c-style operators for bitshifts (since `shiftL` and `shiftR` are too verbose)
(.<<.) :: (Bits a) => a -> Int -> a
(.<<.) a b = a `shiftL` b
(.>>.) :: (Bits a) => a -> Int -> a
(.>>.) a b = a `shiftR` b
