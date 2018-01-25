module Rules where

trp :: (Fractional a, Integral n) => (a -> a) -> n -> (a, a) -> a
trp f n (a, b) = (h / 2) * (f a + 2 * foldl (\p i -> p + f (a + h * fromIntegral i)) 0 [1..n-1] + f b)
    where h = (b - a) / fromIntegral n

mid :: (Fractional a, Integral n) => (a -> a) -> n -> (a, a) -> a
mid f n (a, b) = h * foldl (\p i -> p + f (a + h * (fromIntegral i + 0.5))) 0 [0..n-1]
    where h = (b - a) / fromIntegral n

simp :: (Fractional a, Integral n) => (a -> a) -> n -> (a, a) -> a
simp f n range = (2 * mid f n range + trp f n range) / 3
