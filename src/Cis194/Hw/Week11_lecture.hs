import Control.Applicative

(*>.) :: Applicative f => f a -> f b -> f b
fa *>. fb = liftA2 (const id) fa fb

mapA :: Applicative f => (a -> f b) -> ([a] -> f [b])
mapA f = foldr (liftA2 (:) . f) (pure [])

sequenceA  :: Applicative f => [f a] -> f [a]
sequenceA = foldr (liftA2 (:)) (pure [])

replicateA :: Applicative f => Int -> f a -> f [a]
replicateA n fa = sequenceA (take n (repeat fa))