import System.IO
import System.Environment
import Control.Monad


{- ***** ALGORYTM ***** -}

-- kwadrat to współrzędna i długość boku
newtype Square = Square { unsquare :: (Integer, Integer, Integer) } deriving Show
-- plansza to lista kwadratów
type Board = [Square]
-- punkt to po prostu para złożona z dwóch liczb całkowitych (w moim przypadku)
type Point = (Integer, Integer)
-- pole to współrzędna oraz dodatkowa liczba z treści zadania
type Field = (Integer, Integer, Integer)

-- sprawdza czy dwa kwadraty ze sobą nie kolidują
valid :: Square -> Square -> Bool
valid (Square (ax, ay, ad)) (Square (bx, by, bd)) =
    not $ (vero && hori) || (horo && veri)
  where
    vero = or [ax == bx, ax + ad == bx, ax == bx + bd, ax + ad == bx + bd]
    horo = or [ay == by, ay + ad == by, ay == by + bd, ay + ad == by + bd]
    veri = (ax <= bx && bx <= ax + ad) || (bx <= ax && ax <= bx + bd)
    hori = (ay <= by && by <= ay + ad) || (by <= ay && ay <= by + bd)

-- sprawdza czy kwadrat pasuje do planszy
fit :: Square -> Board -> Bool
fit square = and . map (valid square)

-- zwraca ilość liczb wewnątrz kwadratu
count :: Square -> [Field] -> Integer
count (Square (x, y, d)) =
    foldr inc 0
  where
    inc (x', y', _) s
        | x < x' && x' < x + d && y < y' && y' < y + d = s + 1
        | otherwise                                        = s

-- generuje listę mieszczących się na planszy kwadratów o danym środku
expand :: (Integer, Integer) -> Point -> [Square]
expand (width, height) (x, y) =
    nw ++ ne ++ se ++ sw 
  where
    nw = map (\d -> Square (x - d, y - d, d)) [1..min (x - 1) (y - 1)]
    ne = map (\d -> Square (x, y - d, d)) [1..min (width - x) (y - 1)]
    se = map (\d -> Square (x, y, d)) [1..min (width - x) (height - y)]
    sw = map (\d -> Square (x - d, y, d)) [1..min (x - 1) (height - y)]

-- odpowiada na zagadę z treści zadania
answers :: (Integer, Integer) -> [Field] -> [Board]
answers size fields =
    aux fields
  where
    aux [] = do
        return []
    aux ((x, y, c) : rest) = do
        board <- aux rest
        square <- expand size (x, y)
        guard $ fit square board
        guard $ c == count square fields
        return $ square : board


{- ***** OBSŁUGA WEJŚCIA I WYJŚCIA ***** -}

-- drukuje dane rozwiązanie na ekran
putBoard :: Board -> IO ()
putBoard = putStrLn . show . map unsquare

-- pętla do drukowania pojedyńczych odpowiedzi
step :: [Board] -> IO ()
step [] = do
    putStrLn "Brak rozwiązań."
step (board : rest) = do
    putBoard board
    getLine
    step rest

-- drukuje wszystkie odpowiedzi od razu
every :: [Board] -> IO ()
every [] = do
    return ()
every (board : rest) = do
    putBoard board
    every rest

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    -- sparsuj argumenty i wybierz rodzaj akcji
    args <- getArgs
    let (action, filename) = do
        case args of
            filename : "-step" : _ -> (step, filename)
            filename : _           -> (every, filename)
    -- otwórz plik (trzy wiersze: pierwsze dwa z wymiarami i trzeci z opisem)
    content <- readFile filename
    let width : height : fields : _ = map (filter (/= '.')) $ lines content
    -- wypisz odpowiedzi w zależności od wybranego rodzaju
    action $ answers (read width, read height) (read fields)
