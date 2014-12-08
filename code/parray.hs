import Data.Vector as V
import Control.Parallel.Strategies

main :: IO ()
main = do
    let n = 1000.0
        a = fromList [1.0..n]
        b = V.map fun a `using` parTraversable rseq
    print $ dotp a b

fun :: Float -> Float
fun x = y + 1.0 / exp y
    where
        y = x / sqrt x

dotp :: Vector Float -> Vector Float -> Float
dotp a b = V.sum (V.zipWith (*) a b `using` parTraversable rseq)

