import Data.List

type Field = (Int, Int, Int)

-- przykład z treści zadania
t0 :: [Field]
t0 = [(3, 3, 0), (3, 5, 0), (4, 4, 1), (5, 1, 0), (6, 6, 3)]
t0Size = 7

-- przykład z "Wiedza i Życie"
t1 :: [Field]
t1 = [(2, 11, 3), (3, 5, 2), (3, 10, 1), (4, 3, 0), (4, 4, 1), (5, 9, 1), (6, 10, 1), (7, 3, 0), (9, 5, 2), (10, 3, 0), (10, 6, 1), (10, 11, 1), (11, 7, 4), (11, 10, 1)]
t1Size = 12

-- mały przykład, do skalowania
t3 :: [Field]
t3 = [(2, 2, 0), (2, 3, 0), (3, 2, 0), (3, 3, 0)]
t3Size = 4

-- bierze (kwadratową) planszę wejściową i kopiuje powiększa ją n-krotnie
scale :: Int -> [Field] -> Int -> [Field]
scale size fields n = do
    (x, y, c) <- fields
    tx <- [0..n - 1]
    ty <- [0..n - 1]
    return $ (x + tx * size, y + ty * size, c)

main :: IO ()
main = do
    let n = 8
    putStrLn . show $ (t3Size * n, t3Size * n)
    putStrLn . show $ scale t3Size t3 n