{-# LANGUAGE ParallelListComp #-}

-- CS 3210 - Principles of Programming Languages - Spring 2020
-- Programming Assignment 02 - The N-queens Problem
-- Author: Brian LeProwse
-- Collabrating Author : Evan Birt

import Data.List

type Seq   = [Char]
type Board = [Seq]

-- TODOd 01/17   -- Build nXn Board. Input < 4, build 4x4 Board
setup :: Int -> Board  
setup n = if n < 4 then replicate 4 ([1..4] >> "-") else replicate n ([1..n] >> "-")  

-- TODOd 02/17   -- Return number of rows in supplied Board.
rows :: Board -> Int
rows board = length board

-- TODOd 03/17   -- Takes a Board, returns # of cols if all rows have same # of cols, zero otherwise
cols :: Board -> Int
cols [] = 0
cols board 
  | sum (map length board) `mod` rows board /= 0 = 0 
  | otherwise = sum (map length board) `div` rows board

-- TODOd 04/17   -- Return size of Board (same as # of rows) if cols returns 0.
size :: Board -> Int
size boardSize = if cols boardSize == 0 then 0 else rows boardSize

-- TODOd 05/17   -- Return # of queens found in given sequence ([Char]).
queensSeq :: Seq -> Int 
queensSeq queens = length (filter (=='Q') queens)

-- TODOd 06/17   -- Takes a Board & retuns # of queens found in Board.
queensBoard :: Board -> Int
queensBoard queensOnBoard = sum (map queensSeq queensOnBoard)

-- TODOd 07/17   -- Takes a sequence returns False if sequence has more than 1 queen. True otherwise.
seqValid :: Seq -> Bool
seqValid queens
  | queensSeq queens > 1 = False
  | otherwise = True

-- TODOd 08/17    -- Take Board, return T/F whether ALL of Board rows make valid sequences. 
rowsValid :: Board -> Bool
rowsValid board 
 | (cols board > 0 && length [x | x <- board, seqValid x] == length board) = True
 | otherwise = False

-- TODOd 09/17    -- Take Board, return T/F whether ALL of Board cols make valid sequence.
colsValid :: Board -> Bool
colsValid board 
  | rowsValid (transpose board) = True
  | otherwise = False

-- TODOd 10/17    -- Take Board, return # of diagonals.
diagonals :: Board -> Int
diagonals [] = 0
diagonals board = 2 * (size board)-1

mainDiagIndices :: Board -> Int -> [ (Int, Int) ]
mainDiagIndices b p
  | p < n = [ (n - 1 - qr, q) | q <- [0..p] | qr <- [p,p-1..0] ]
  | otherwise = [ (q, (n - 1 - qr)) | q <- [0..2 * (n - 1) - p] | qr <- [2 * (n - 1) - p,2 * (n - 1) - p - 1..0] ]
  where n = size b

-- TODOd 11/17    -- Take Board, return a list of all primary diagonal indices.
-- Collabrating Author : Evan Birt
allMainDiagIndices :: Board -> [[ (Int, Int) ]]
allMainDiagIndices board = [mainDiagIndices (board) index | index <- [0..diagonals(board)-1]]

-- TODOd 12/17    -- Take Board, return list of all primary diag elements. 
-- Collabrating Author : Evan Birt
mainDiag :: Board -> [Seq]
mainDiag board = ( map . map ) (\(x,y) ->  (board !! x) !! y) (allMainDiagIndices(board))

secDiagIndices :: Board -> Int -> [ (Int, Int) ]
secDiagIndices b p
  | p < n = [ (p - q, q) | q <- [0..p] ]
  | otherwise = [ (p - (n - 1 - q), n - 1 - q) | q <- [2 * (n - 1) - p, 2 * (n - 1) - p - 1..0] ]
  where n = size b

-- TODOd 13/17    -- Take Board, return list of all secondary diagonal indices. 
-- Collabrating Author : Evan Birt
allSecDiagIndices :: Board -> [[ (Int, Int) ]]
allSecDiagIndices board = [secDiagIndices (board) index | index <- [0 .. diagonals(board)-1]]

-- TODOd 14/17    -- Take Board, return list of all secondary diag. elements.
-- Collabrating Author : Evan Birt
secDiag :: Board -> [Seq]
secDiag board = ( map . map ) (\(x,y) ->  (board !! x) !! y) (allSecDiagIndices(board))
  
-- TODOd 15/17    -- Take a Board, return T/F if all primary and secondary diags valid.
diagsValid :: Board -> Bool
diagsValid board 
  | and (map seqValid (mainDiag board)) && 
    and (map seqValid (secDiag board)) == True = True
  | otherwise = False

-- TODOd 16/17    -- Take Board, return T/F if Board configuration is valid. 
valid :: Board -> Bool
valid board
  | and (map seqValid (mainDiag board)) && and (map seqValid (secDiag board)) && 
    colsValid board && rowsValid board == True = True
  | otherwise = False

-- Take Board, return T/F if Board config is solved. 
--( Board is valid & has correct amount of queens based on Board size.)
-- TODO 17/17 (¡Phew!)  
solved :: Board -> Bool
solved board = if valid board && 
                  (queensBoard board == size board) == True then True else False

setQueenAt :: Board -> Int -> [Board]
setQueenAt b i = do
  let z = replicate ((size b) - 1) '-'
  let p = nub (permutations ("Q" ++ z))
  [ [ (b!!k) | k <- [0..(i-1)] ] ++ [r] ++ [ (b!!k) | k <- [(i+1)..((rows b) - 1)] ] | r <- p ]

nextRow :: Board -> Int
nextRow b = head [ i | i <- [0 .. (size b) - 1], queensSeq (b!!i) == 0 ]

solve :: Board -> [Board]
solve b
  | solved b = [b]
  | otherwise = concat [ solve newB | newB <- setQueenAt b i, valid newB ]
    where i = nextRow b

main = do
  -- length of solution list = # of solutions for each n size Board.
  -- Tested with setup n values: 0 <= n <= 9 (n = 9 took foreverrrr). 
  -- (Size n Board = # of possible Board solutions)
  --  0-4 = 2, 5 = 10, 6 = 4, 7 = 40, 8 = 92, 9 = 352
  let b = setup 6
  let solution = [ solution | solution <- solve b ]
  -- TEST let solution = length [ solution | solution <- solve b ]
  print (solution)
