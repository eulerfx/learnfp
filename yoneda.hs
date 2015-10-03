{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

type Machine1 a = forall b . (a -> b) -> b 

machine1 :: Machine1 a
machine1 f = f a where a = undefined -- gets an a somewhere

-- given a machine, produce an a using the identity function
uncheck1 :: (forall b . (a -> b) -> b) -> a
uncheck1 t = t id

-- given an a (as output from unchec1 for example) produce (or emulate) a machine
check1 :: a -> (forall b . (a -> b) -> b)
check1 a f = f a

-- check1 . uncheck1 = id


-- a machine that takes a function a -> b and produces a list of b
type Machine2 a = forall b . (a -> b) -> [b]

machine2 :: Machine2 a
machine2 f = map f a where a = undefined -- gets an [a] somewhere

uncheck2 :: (forall b . (a -> b) -> [b]) -> [a]
uncheck2 t = t id

check2 :: [a] -> (forall b . (a -> b) -> [b])
check2 a f = map f a


machine2' :: Machine2 a
machine2' f = reverse $ map f a where a = undefined


-- a machine which when give a function a -> b, produces a function c -> b for some type c
type Machine3 a c = forall b . (a -> b) -> (c -> b)

machine3 :: Machine3 a c
machine3 f = f . a where a x = undefined -- machine stores a function a::c -> a which it composes with f

uncheck3 :: Machine3 a c -> (c -> a)
uncheck3 t = t id

check3 :: (c -> a) -> Machine3 a c
check3 a f = f . a




data I a = I a

instance Functor I where
	fmap f (I a) = I (f a)


--instance Functor ((->) a) where
--	fmap f = (.) f

-- given a functor-full of a, and a function a -> b, get back a functor-full of b
-- in other words, if we have an f a (which gives us fmap : (a -> b) -> f b) and are given a function a -> b, we can get an f b (by simply using fmap)
check :: Functor f => f a -> (forall b . (a -> b) -> f b)
check a f = fmap f a

-- given a machine that takes a function a -> b and produces a functor-full of b, produce a functor-full of a
-- this is done by substituting id for a -> b
-- in other words, if we have a mapping from a -> b to an f b, we can get an f a (by passing it id which exists by definition)
uncheck :: (forall b . (a -> b) -> f b) -> f a
uncheck t = t id

-- existential and universal quantification are left and right adjoints of substitution

-- uncheck (check f) 
-- 	= (check f) id
-- 	= fmap id f
-- 	= id f
-- 	= f		








main = putStrLn "yoneda"
