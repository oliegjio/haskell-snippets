import Data.List

integrate :: (Float -> Float) -> Float -> Float -> Float -> Float
integrate f x0 x1 dx = foldl (\ a x -> a + dx * f x) 0 (takeWhile (<= x1) [x0, x0 + dx ..])

matrixAddition :: [[Float]] -> [[Float]] -> [[Float]]
matrixAddition = zipWith $ zipWith (+)

matrixMultiplication :: [[Float]] -> [[Float]] -> [[Float]]
matrixMultiplication a b = [[sum $ zipWith (*) ar bc | bc <- transpose b] | ar <- a]

main :: IO ()
main = do
    print $ integrate (**2) 0 1 0.001
    print $ matrixAddition [[1, 2], [3, 4]] [[4, 3], [2, 1]]
    print $ matrixMultiplication [[1, 2], [3, 4]] [[4, 3], [2, 1]]
