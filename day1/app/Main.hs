module Main where
import           Data.List                      ( sort )
import           Data.List.Split                ( splitOn )
import           Text.Read                      ( readMaybe )

inputFile :: String
inputFile = "input1.txt"

main :: IO ()
main = do
  input <- readFile inputFile
  case elfCalories input of
    Nothing -> putStrLn "Failed to parse"
    Just inp ->
      putStrLn
        $  "Max calorie = "
        ++ show (maxElfCalorie inp)
        ++ ", Top Three Calorie = "
        ++ show (topThreeSum inp)

elfCalories :: String -> Maybe [[Int]]
elfCalories inp = mapM (mapM readMaybe) elfLines
 where
  mLines   = lines inp
  elfLines = splitOn [""] mLines

maxElfCalorie :: [[Int]] -> Int
maxElfCalorie = maximum . map sum

topThreeSum :: [[Int]] -> Int
topThreeSum = sum . take 3 . reverse . sort . map sum
