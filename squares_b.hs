import System.IO
import System.Environment
import Control.Monad
import Data.Array


{- ***** ALGORYTM ***** -}

-- kwadrat to współrzędna i długość boku
newtype Square = Square { unsquare :: (Int, Int, Int) } deriving Show
-- plansza to lista kwadratów
type Board = [Square]
-- punkt to po prostu para złożona z dwóch liczb całkowitych (w moim przypadku)
type Point = (Int, Int)
-- pole to współrzędna oraz dodatkowa liczba z treści zadania
type Field = (Int, Int, Int)

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



-- odpowiada na zagadę z treści zadania
answers :: (Int, Int) -> [Field] -> [Board]
answers size@(width, height) fields =
    aux fields
  where
    aux [] = do
        return []
    aux (f:fs) = do
        board <- aux fs
        square <- expand f
        guard $ fit square board
        return $ square : board
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
