import qualified Data.ByteString.Lazy as BL (writeFile)
import           Data.Csv
import           Numeric              (showFFloat)

data Estimate = Estimate { n             :: Integer
                         , trapezium     :: Double
                         , midpoint      :: Double
                         , simpsons      :: Double
                         , difference    :: Maybe Double
                         , ratio         :: Maybe Double
                         , extrapolation :: Maybe Double
                         } deriving Show

estimate :: (Double -> Double) -> Integer -> (Double, Double) -> Estimate
estimate f m (a, b) = Estimate m trp mid simp Nothing Nothing Nothing
  where h = (b - a) / fromInteger m
        trp = (h / 2) * (f a + 2 * foldl (\p i -> p + f (a + h * fromIntegral i)) 0 [1..m-1] + f b)
        mid = h * foldl (\p i -> p + f (a + h * (fromIntegral i + 0.5))) 0 [0..m-1]
        simp = (2 * mid + trp) / 3

applyToNext :: (a -> a -> b) -> [a] -> [b]
applyToNext o l = zipWith o (tail l) (init l)

applyToNextRes :: (Estimate -> a) -> (a -> a -> b) -> (Estimate -> b -> Estimate) -> [Estimate] -> [Estimate]
applyToNextRes getter f setter es@(e:_) = e : applyToNext (\res2 res1 -> setter res2 $ f (getter res2) (getter res1)) es
applyToNextRes _      _ _      []       = []

diffs :: [Estimate] -> [Estimate]
diffs = applyToNextRes simpsons (-) (\est d -> est { difference = Just d })

ratios :: [Estimate] -> [Estimate]
ratios = applyToNextRes difference (\d2 d1 -> (/) <$> d2 <*> d1) (\est r -> est { ratio = r })

inf :: [Estimate] -> [Estimate]
inf = applyToNextRes simpsons (\s2n sn -> s2n - (s2n - sn) / 15) (\est extr -> est { extrapolation = Just extr })

showD :: Double -> String
showD x = showFFloat (Just 15) x ""

func :: Double -> Double
func x = sin x / log x + 1

instance ToRecord Estimate where
    toRecord (Estimate i t m s d r e) =
      record [ toField i
             , toField $ showD t
             , toField $ showD m
             , toField $ showD s
             , toField $ showD <$> d
             , toField $ showD <$> r
             , toField $ showD <$> e
             ]

range :: (Double, Double)
range = (4, 8)

main :: IO ()
main = BL.writeFile file $ encode $ inf $ ratios $ diffs $ (\i -> estimate func i range) . (2^) <$> [1..6]
  where file = "results.csv"
