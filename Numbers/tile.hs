import System.Environment(getArgs)

tileCost ::  Float -> Float -> Float -> Float
tileCost price width height = price * width * height

main :: IO ()
main = do
  [price, width, height] <- getArgs
  print $ tileCost (read price) (read width) (read height)
