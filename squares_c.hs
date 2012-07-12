import System.IO
import System.Environment
import Control.Monad
import Data.Array
import Data.Map (Map)
import qualified Data.Map as Map


{- ***** ALGORYTM ***** -}

-- kwadrat to współrzędna i długość boku
newtype Square = Square { unsquare :: (Int, Int, Int) } deriving Show
-- przedział to para: (początek przedziału, koniec przedziału)
type Range = (Int, Int)
-- dwie mapy zawierające listy z przedziałami do efektywnego wyszukiwania kolizji
type Space = (Map Int [Range], Map Int [Range])
-- plansza to lista kwadratów oraz
type Board = ([Square], Space)
-- punkt to po prostu para złożona z dwóch liczb całkowitych (w moim przypadku)
type Point = (Int, Int)
-- pole to współrzędna oraz dodatkowa liczba z treści zadania
type Field = (Int, Int, Int)


-- buduje tablicę do efektywnego liczenia ilości pól w kwadracie
buildArray :: (Int, Int) -> [Field] -> Array (Int, Int) Int
buildArray (width, height) fields =
    a
  where
    -- lita pól: [(x, y, c)] zamieniona na listę indeksów z 1: [((x, y), 1)]
    bitted = map (\(x, y, _) -> ((x, y), 1)) fields
    -- tablica bitów stworzona na podstawie powyższej listy
    b = accumArray (+) 0 ((0, 0), (width, height)) bitted
    -- tablica finalna - preprocesing na powyższej tablicy
    a = array ((0, 0), (width, height)) $
        [((x, 0), 0) | x <- [0..width]] ++
        [((0, y), 0) | y <- [0..height]] ++
        [((x, y), v x y) | x <- [1..width], y <- [1..height]]
    -- wartość w tablicy na indeksie (x, y) (znany wzór)
    v x y = b ! (x, y) + a ! (x - 1, y) + a ! (x, y - 1) - a ! (x - 1, y - 1)

-- zwraca ilość liczb wewnątrz kwadratu
count :: Array (Int, Int) Int -> Square -> Int
count a (Square (x, y, d)) =
    a ! (dx, dy) - a ! (dx, y - 1) - a ! (x - 1, dy) + a ! (x - 1, y - 1) - 1
  where
    dx = x + d
    dy = y + d


-- sprawdza czy dwa przedziały mają biejsce wspólne
overlap :: Range -> Range -> Bool
overlap (ab, ae) (bb, be) =
    (ab <= be) && (bb <= ae)

-- sprawdza czy kwadrat pasuje do danej przestrzeni i dodaje go
fit :: Square -> Space -> (Bool, Space)
fit (Square (x, y, d)) (hor, ver) =
    let (bx, lx) = checkHor (Map.lookup x hor)
        (by, ly) = checkVer (Map.lookup y ver)
        (bxd, lxd) = checkHor (Map.lookup (x + d) hor)
        (byd, lyd) = checkVer (Map.lookup (y + d) ver)
        bool = not (bx || by || bxd || byd)
        newHor = Map.insert x lx . Map.insert (x + d) lxd $ hor
        newVer = Map.insert y ly . Map.insert (y + d) lyd $ ver
    in (bool, (newHor, newVer))
  where
    ry = (y, y + d)
    rx = (x, x + d)
    checkHor Nothing = (False, [ry])
    checkHor (Just list) = (any (overlap ry) list, ry : list)
    checkVer Nothing = (False, [rx])
    checkVer (Just list) = (any (overlap rx) list, rx : list)

-- odpowiada na zagadę z treści zadania
answers :: (Int, Int) -> [Field] -> [Board]
answers size@(width, height) fields =
    aux fields
  where
    aux [] = do
        return ([], (Map.empty, Map.empty))
    aux (f:fs) = do
        (board, space) <- aux fs
        square <- expand f
        let (fits, newSpace) = square `fit` space
        guard fits
        return $ (square : board, newSpace)
    count' = count $ buildArray size fields
    -- generuje listę mieszczących się na planszy kwadratów o danym środku
    expand :: Field -> [Square]
    expand (x, y, c) =
        sieve nw ++ sieve ne ++ sieve se ++ sieve sw 
      where
        nw = map (\d -> Square (x - d, y - d, d)) [1..min (x - 1) (y - 1)]
        ne = map (\d -> Square (x, y - d, d)) [1..min (width - x) (y - 1)]
        se = map (\d -> Square (x, y, d)) [1..min (width - x) (height - y)]
        sw = map (\d -> Square (x - d, y, d)) [1..min (x - 1) (height - y)]
        sieve = (takeWhile $ (== c) . count') . (dropWhile $ (< c) . count')


{- ***** OBSŁUGA WEJŚCIA I WYJŚCIA ***** -}

-- drukuje dane rozwiązanie na ekran
putBoard :: Board -> IO ()
putBoard = putStrLn . show . map unsquare . fst

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
