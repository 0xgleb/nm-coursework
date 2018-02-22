import qualified Data.ByteString.Lazy as BL (writeFile)
import           Data.Csv
import           Numeric              (showFFloat)

{-| A new data type for saving estimates -}
data Estimate = Estimate { n             :: Integer      -- ^ n
                         , trapezium     :: Double       -- ^ estimate by the trapezium rule
                         , midpoint      :: Double       -- ^ estimate by the midpoint rule
                         , simpsons      :: Double       -- ^ estimate by Simpson's rule
                         , difference    :: Maybe Double -- ^ S_n - S_{n/2}
                         , ratio         :: Maybe Double -- ^ (S_n - S_{n/2}) / (S_{n/2} - S_{n/4})
                         , extrapolation :: Maybe Double -- ^ S_n + (S_n - S_{n/2})/15
                         } deriving Show

{-| This function takes a function that we want to integrate, how many strips we want to use
and a range. It calculates three estimates using the trapezium, midpoint and Simpson's rules.
It returns the estimates using the Estimate data type.
-}
estimate :: (Double -> Double) -> Integer -> (Double, Double) -> Estimate
estimate f m (a, b) = Estimate m trp mid simp Nothing Nothing Nothing
  where h = (b - a) / fromInteger m -- length of a strip
        trp = (h / 2) * (f a + 2 * foldl (\p i -> p + f (a + h * fromIntegral i)) 0 [1..m-1] + f b)
        -- the trapezium rule
        mid = h * foldl (\p i -> p + f (a + h * (fromIntegral i + 0.5))) 0 [0..m-1]
        -- the midpoint rule
        simp = (2 * mid + trp) / 3
        -- Simpson's rule

{-| This function takes a function f that with two arguments and a list [x_1, x_2, ..., x_n].
It returns the list [f(x_2, x_1), f(x_3, x_2), ..., f(x_n, x_{n-1})].
-}
applyToNext :: (a -> a -> b) -> [a] -> [b]
applyToNext o l = zipWith o (tail l) (init l)

{-| This function takes a function that extracts a value from an estimate g, a function f with two
arguments, a function that updates an estimate s, a list [e_1, ..., e_n] and returns
[e_1, s(e_2, f(g(e_2), (e_1))), s(e_3, f(g(e_3), (e_2))), ..., s(e_n, f(g(e_n), (e_{n-1})))]
-}
applyToNextRes :: (e -> a) -> (a -> a -> b) -> (e -> b -> e) -> [e] -> [e]
applyToNextRes _      _ _      []       = []
applyToNextRes getter f setter es@(e:_) =
    e : applyToNext (\res2 res1 -> setter res2 $ f (getter res2) (getter res1)) es

{-| This function takes a list of estimates and returns the same estimates, but
with the difference field updated for each estimate (except the first one)
-}
diffs :: [Estimate] -> [Estimate]
diffs = applyToNextRes simpsons (-) (\est d -> est { difference = Just d })

{-| This function takes a list of estimates and returns the same estimates, but
with the ratio field updated for each estimate (except for the first two)
-}
ratios :: [Estimate] -> [Estimate]
ratios = applyToNextRes difference (\d2 d1 -> (/) <$> d2 <*> d1) (\est r -> est { ratio = r })

{-| This function takes a list of estimates and returns the same estimates, but
with the extrapolation field updated for each estimate (except for the first one)
-}
inf :: [Estimate] -> [Estimate]
inf = applyToNextRes simpsons (\s2n sn -> s2n + (s2n - sn) / 15) setter
      where setter est extr = est { extrapolation = Just extr } -- updates the extrapolation field

{-| This function takes a number and converts it to a string with the number up to 15 d.p. -}
showD :: Double -> String
showD x = showFFloat (Just 15) x ""

{-| This is the function that we want to integrate -}
func :: Double -> Double
func x = sin x / log x + 1

{-| Estimate is made an instance of the ToRecord type class
so that we can write estimates to a csv file
-}
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

{-| Range of the integral -}
range :: (Double, Double)
range = (4, 8)

{-| Main procedure. This is an IO action that finds estimates, differences, ratios,
and extrapolations for values of n which are the first 6 powers of 2.
Then it writes all the estimates into a csv file.
-}
main :: IO ()
main = BL.writeFile file $ encode $ inf $ ratios $ diffs $ nToEstimate . (2^) <$> [1..6]
  where nToEstimate i = estimate func i range
        file = "results.csv"
