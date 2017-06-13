module Main where

import Control.Monad
import Data.Monoid ((<>))
import Data.List (partition)

type Tuple = (String, String)
type Backlog = Tuple
type Steps = [Tuple]
type Solutons = ([(Tuple, Steps)], [(Tuple, Steps)])

tuples :: [Tuple]
tuples = [
--    ("00", "1"),
--    ("1", "0"),
--    ("0", "01"),
--    ("110", "1"),
--    ("0", "010"),
--    ("01", "00"),
--    ("01", "00")

--   ("001", "0"),
--   ("01", "011"),
--   ("01", "101"),
--   ("10", "001")

--  ("0", "010"),
--  ("1", "101"),
--  ("0101", "01")

  ("10", "0"),
  ("0", "100"),
  ("001", "0"),
  ("0", "01")
  ]

isValid :: Tuple -> Bool
isValid = uncurry go
  where
    go "" _ = True
    go _ "" = True
    go (x:xs) (y:ys) = x==y && go xs ys

reduceTuple :: Tuple -> Tuple
reduceTuple t@("", _) = t
reduceTuple t@(_, "") = t
reduceTuple t@((x:xs), (y:ys))
  | x==y = reduceTuple (xs, ys)
  | otherwise = t

combine :: ([a], [b]) -> ([a], [b]) -> ([a], [b])
combine (a, b) (c,d) = (a <> c, b <> d)

next :: Backlog -> [(Backlog, Tuple)]
next backlog = do
  t <- tuples
  let backlog' = reduceTuple (combine backlog t)
  guard (isValid backlog')
  return (backlog', t)

isEmpty :: Tuple -> Bool
isEmpty = (==) ("", "")

addSteps :: Steps -> [(a, Tuple)] -> [(a, Steps)]
addSteps steps = fmap (\(b, t) -> (b, t:steps))

findSolutions :: [(Tuple,Steps)] -> Solutons
findSolutions = partition (\(b, t) -> isEmpty b)

nextSolutions :: Backlog -> Steps -> Solutons
nextSolutions backlog steps = findSolutions (addSteps steps (next backlog))

continueSolutions :: [(Backlog, Steps)] -> Solutons
continueSolutions bs =
  let bs' = fmap (uncurry nextSolutions) bs
  in foldl combine ([], []) bs'

solvePrefixes :: [(Backlog, Steps)] -> [Steps]
solvePrefixes bs =
  let (solutions, continuations) = continueSolutions bs
      solutionSteos = fmap (reverse . snd) solutions
  in  solutionSteos <> solvePrefixes continuations

solvePrefix :: Backlog -> Steps -> [Steps]
solvePrefix backlog steps = solvePrefixes [(backlog, steps)]

main :: IO ()
main = print (head (solvePrefix ("", "") []))
