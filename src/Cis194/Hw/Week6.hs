fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = (fib (n-1)) + (fib (n-2))

fibs2 :: [Integer]
fibs2 = (map fst) . (iterate (\(x,y) -> (y,x+y))) $ (0,1)

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs 

instance Show a => Show (Stream a) where
  show = show . (take 20) . streamToList

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) ys = Cons x (interleaveStreams ys xs)

ruler :: Stream Integer
ruler = ruler' 0
  where ruler' n = interleaveStreams (streamRepeat n) (ruler' (n+1))