module Main (main) where
import Data.List (unfoldr)
import System.Environment (getArgs)
 
primes :: [Integer]
primes = 2 : 3 : primes'
  where 1 : (p : candidates) = [6 * k + r | k <- [0 ..], r <- [1, 5]]
        primes' = p : filter isPrime candidates
        isPrime n = all (notDiv n) $ takeWhile (\ x -> x * x <= n) primes'
        notDiv a b = a `mod` b /= 0
 
primeFactors :: Integer -> [Integer]
primeFactors = unfoldr factor
  where isDiv x y = x `mod` y == 0
        factor 1 = Nothing
        factor j
          = (\ (x : _) -> Just (x, j `div` x)) $ filter (isDiv j) primes
 
main :: IO ()
main
  = do [start] <- getArgs
       print $ primeFactors $ read start
