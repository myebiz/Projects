import System.Environment(getArgs)

fibs :: (Num a, Ord a) => a -> a
fibs = fibs' 0 1

fibs' ::  (Num a, Num a1, Ord a1) => a -> a -> a1 -> a
fibs' prev1 prev2 start
  | start <= 0 = next
  | otherwise = fibs' prev2 next (start - 1)
  where next = prev1 + prev2

fibseq ::  (Enum t, Num t, Ord t) => t -> [t]
fibseq n = [fibs x | x <- [0..n-1]]

main :: IO ()
main = do
  args <- getArgs
  let f = read (head args) :: Int
  print $ fibseq f
