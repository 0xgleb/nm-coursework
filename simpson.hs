import qualified Data.ByteString.Lazy as BL (writeFile)
import           Data.Csv             (encode)
import           Numeric              (showFFloat)

trp :: (Fractional a) => (a -> a) -> Integer -> (a, a) -> a
trp f n (a, b) =
  (h / 2) * (f a + 2 * foldl (\p i -> p + f (a + h * fromIntegral i)) 0 [1..n-1] + f b)
  where h = (b - a) / fromInteger n

mid :: (Fractional a) => (a -> a) -> Integer -> (a, a) -> a
mid f n (a, b) = h * foldl (\p i -> p + f (a + h * (fromIntegral i + 0.5))) 0 [0..n-1]
  where h = (b - a) / fromInteger n

simp :: (Fractional a) => (a -> a) -> Integer -> (a, a) -> a
simp f n range = (2 * mid f n range + trp f n range) / 3

diffs :: Fractional b => [(a, b)] -> [(a, b, b, b)]
diffs = diffs3 . diffs2
  where diffs2 []                 = []
        diffs2 [(a, b)]           = [(a, b, 0)]
        diffs2 ((a, b):(c, d):xs) =
          (a, b, 0) : (\((x, y, _):ds) -> (x, y, d-b) : ds) (diffs2 $ (c, d) : xs)

        diffs3 []                       = []
        diffs3 [(a, b, c)]              = [(a, b, c, 0)]
        diffs3 ((a, b, c):(d, e, f):xs) =
          (a, b, c, 0) : (\((x, y, z, _):ds) -> (x, y, z, f/c) : ds) (diffs3 $ (d, e, f) : xs)

inf :: Fractional b => [(a, b, c, d)] -> [(a, b, c, d, b)]
inf []                             = []
inf [(a, b, c, d)]                 = [(a, b, c, d, 0)]
inf ((a, b, c, d):(e, f, g, h):xs) =
  (a, b, c, d, 0) : (\((v,x,y,z,_):ds) -> (v,x,y,z,infExtr f b) : ds) (inf $ (e, f, g, h) : xs)
  where infExtr x y = y - (y - x) / 15

showD :: Double -> String
showD x = showFFloat (Just 15) x ""

func :: Double -> Double
func x = sin x / log x + 1

main :: IO ()
main =  BL.writeFile file $ encode $ show7 . (\(a, b, c, d, e) -> (a, trp func a (4, 8), mid func a (4, 8), b, c, d, e))
    <$> inf (diffs $ (\i -> (i, simp func i (4,8))) . (2^) <$> [1..6])
  where show7 (a, b, c, d, e, f, g) = (a, showD b, showD c, showD d, showD e, showD f, showD g)
        file = "results.csv"
