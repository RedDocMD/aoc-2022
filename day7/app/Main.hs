{-# LANGUAGE LambdaCase #-}
module Main where
import           Data.Foldable                  ( foldl' )
import           Data.List                      ( isPrefixOf )
import qualified Data.Map                      as M
import           Text.Read                      ( readMaybe )


filename :: String
filename = "input1.txt"

main :: IO ()
main = do
    input <- readFile filename
    let fLines     = parseFile input
        dMap       = maybe M.empty processInput fLines
        sizes      = map (findSize dMap) (M.keys dMap)
        sum1       = sum $ filter (< 100000) sizes
        totSize    = 70000000
        targetSize = 30000000
        toFreeSize = max 0 targetSize - (totSize - maximum sizes)
        delSize    = minimum $ filter (>= toFreeSize) sizes
    putStrLn $ "Sum of sizes of directories with size < 100000 = " ++ show sum1
    putStrLn
        $  "Size of minimum sized directory to delete to free at least "
        ++ show toFreeSize
        ++ " = "
        ++ show delSize


data Command = Cd CdCommand | Ls deriving Show

data CdCommand = Into String | Up | Root deriving Show

data Listing= FileListing String Int | DirectoryListing String deriving Show

type LineResult = Either Command Listing

parseFile :: String -> Maybe [LineResult]
parseFile = mapM parseLine . lines

parseLine :: String -> Maybe LineResult
parseLine line | "$ " `isPrefixOf` line = Left <$> parseCommand (drop 2 line)
               | otherwise              = Right <$> parseListing line

parseCommand :: String -> Maybe Command
parseCommand line
    | "cd " `isPrefixOf` line = Just (Cd (parseCdCommand $ drop 3 line))
    | line == "ls"            = Just Ls
    | otherwise               = Nothing

parseCdCommand :: String -> CdCommand
parseCdCommand "/"  = Root
parseCdCommand ".." = Up
parseCdCommand dir  = Into dir

parseListing :: String -> Maybe Listing
parseListing line
    | "dir " `isPrefixOf` line = Just (DirectoryListing $ drop 4 line)
    | otherwise                = parseFileListing $ words line

parseFileListing :: [String] -> Maybe Listing
parseFileListing [size, name] = FileListing name <$> readMaybe size
parseFileListing _            = Nothing


type Path = [String]
type DirMap = M.Map Path [Listing]

data ProcessState = ProcessState
    { dirMap     :: DirMap
    , currentDir :: Path
    }

processLine :: ProcessState -> LineResult -> ProcessState
processLine st (Left  cmd) = processCommand st cmd
processLine st (Right lst) = st
    { dirMap = M.alter
                   (\case
                       Just x' -> Just $ x' ++ [lst]
                       Nothing -> Just [lst]
                   )
                   (currentDir st)
                   (dirMap st)
    }

processCommand :: ProcessState -> Command -> ProcessState
processCommand st (Cd cmd) = processCd st cmd
processCommand st Ls       = st

processCd :: ProcessState -> CdCommand -> ProcessState
processCd st Up         = st { currentDir = init $ currentDir st }
processCd st Root       = st { currentDir = [] }
processCd st (Into dir) = st { currentDir = currentDir st ++ [dir] }

emptyProcessState :: ProcessState
emptyProcessState = ProcessState { dirMap = M.empty, currentDir = [] }

processInput :: [LineResult] -> DirMap
processInput = dirMap . foldl' processLine emptyProcessState


findSize :: DirMap -> Path -> Int
findSize mp pt =
    let lst = mp M.! pt
        sz (DirectoryListing name) = findSize mp (pt ++ [name])
        sz (FileListing _ sz'    ) = sz'
    in  sum $ map sz lst
