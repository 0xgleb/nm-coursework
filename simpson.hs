import qualified Data.ByteString.Lazy as BL (writeFile)
import           Data.Csv             (encode)

trp :: (Fractional a) => (a -> a) -> Integer -> (a, a) -> a
trp f n (a, b) = (h / 2) * (f a + 2 * foldl (\p i -> p + f (a + h * fromIntegral i)) 0 [1..n-1] + f b)
    where h = (b - a) / fromInteger n

mid :: (Fractional a) => (a -> a) -> Integer -> (a, a) -> a
mid f n (a, b) = h * foldl (\p i -> p + f (a + h * (fromIntegral i + 0.5))) 0 [0..n-1]
    where h = (b - a) / fromInteger n

simp :: (Fractional a) => (a -> a) -> Integer -> (a, a) -> a
simp f n range = (2 * mid f n range + trp f n range) / 3

func :: Double -> Double
func x = sin x / log x + 1

main :: IO ()
main = BL.writeFile "results.csv" $ encode $ (\i -> (i, simp func i (4, 8))) . (2 ^) <$> [1..6]

