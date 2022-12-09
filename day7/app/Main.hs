{-# LANGUAGE LambdaCase #-}
module Main where
import           Data.Foldable                  ( foldl' )
import           Data.List                      ( isPrefixOf )
import qualified Data.Map                      as M
import           Text.Read                      ( readMaybe )


filename :: String
filename = "test.txt"

main :: IO ()
main = do
    input <- readFile filename
    let fLines = parseFile input
        dMap   = maybe M.empty processInput fLines
        sizes  = map (findSize dMap) (M.keys dMap)
        sizes' = M.elems $ sizeMap $ foldl' findSizeC
                                            (emptySizeState dMap)
                                            (M.keys dMap)
        sum1       = sum $ filter (< 100000) sizes
        totSize    = 70000000
        targetSize = 30000000
        toFreeSize = max 0 targetSize - (totSize - maximum sizes)
        delSize    = minimum $ filter (>= toFreeSize) sizes
    print sizes
    print sizes'
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


type SizeMap = M.Map Path Int
data SizeState = SizeState
    { sDirMap :: DirMap
    , sizeMap :: SizeMap
    }

emptySizeState :: DirMap -> SizeState
emptySizeState dm = SizeState { sDirMap = dm, sizeMap = M.empty }

findSizeC' :: SizeState -> Path -> (SizeState, Int)
findSizeC' st pt =
    let
        lst = sDirMap st M.! pt
        sz st' (DirectoryListing name) = findSizeC' st' (pt ++ [name])
        sz st' (FileListing _ sz'    ) = (st', sz')
        acc (st', vals) l =
            let (st'', val) = sz st' l in (st'', vals ++ [val])
        (st', sizes) = foldl' acc (st, []) lst
        size         = sum sizes
        st''         = st' { sizeMap = M.insert pt size (sizeMap st') }
    in
        (st'', sum sizes)

findSizeC :: SizeState -> Path -> SizeState
findSizeC st pt = fst $ findSizeC' st pt
