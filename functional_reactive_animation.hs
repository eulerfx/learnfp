{-# LANGUAGE MultiParamTypeClasses, TypeOperators
           , TypeFamilies, UndecidableInstances, CPP
           , FlexibleContexts #-}


type Time = Float

--data Time deriving (Ord)
--data R = NegInf | R Float | PosInf deriving (Eq,Ord)	
--data Time = T R | AtLeast R


-- semantically, a behavior is a time dependant function
type Behavior a = Time -> a

-- semantically, an event is a list of time value pairs
type Event a = (Time,a)

-- interpretation of a behavior at a point in time
-- this gives the denotation (semantic function)
at :: Behavior a -> Time -> a
at b t = b t

-- interpretation of an event
occ :: Event a -> (Time,a)
occ = id

occs :: Event a -> [(Time,a)]
occs e = undefined

time :: Behavior Time
time = at id

fmap :: (a -> b) -> Behavior a -> Behavior b
fmap f b t = f (b t)

always :: a -> Behavior a
always = const

ap :: Behavior (a -> b) -> Behavior a -> Behavior b
ap bf ba = \t -> (bf t) (ba t)

lift :: (a -> b) -> Behavior a -> Behavior b
lift f a = always f `ap` a

lift2 :: (a -> b -> c) -> Behavior a -> Behavior b -> Behavior c
lift2 f a b = always f `ap` a `ap` b

lift3 :: (a -> b -> c -> d) -> Behavior a -> Behavior b -> Behavior c -> Behavior d
lift3 f a b c = always f `ap` a `ap` b `ap` c

-- time transformtion
timeX :: Behavior a -> Behavior Time -> Behavior a
timeX ba bt = ba . bt

-- timeX b time = b
-- timeX b (time/2) -- slows down animation by factor of 2
-- timeX b (time - 2) -- delays by 2 seconds


class AdditiveGroup v where
	zeroV :: v
	(^+^) :: v -> v -> v
	negateV :: v -> v
	(^-^) :: v -> v -> v

class AdditiveGroup v => VectorSpace v where
	type Scalar v :: *
	(*^) :: Scalar v -> v -> v


-- ∫{t,t0} b
-- can be used to specify velocity and acceleration
-- creates a behavior which sums all values of the argument behavior starting with 
-- the initial time up to the argument time
-- the vector space implements the summation
integral :: VectorSpace a => Behavior a -> Time -> Behavior a
integral b t0 t = b t ^+^ b t0


-- returns values of argument behavior until event occurs
-- after which the values of the behavior produced by the event are returned
untilB :: Behavior a -> Event (Behavior a) -> Behavior a
untilB b e t = if t <= te then b t else b' t
	where (te,b') = occ e



(+=>) :: Event a -> (Time -> a -> b) -> Event b
e +=> f = (te,f te a) where (te,a) = occ e

(==>) :: Event a -> (a -> b) -> Event b
e ==> f = (te,f a) where (te,a) = occ e

(*=>) :: Event a -> (Time -> b) -> Event b
e *=> f = e +=> \t _ -> f t

(-=>) :: Event a -> b -> Event b
e -=> b = e +=> \_ _ -> b


constEv :: Time -> a -> Event a
constEv t a = (t,a)


lbp :: Time -> Event (Event ())
lbp = undefined

rbp :: Time -> Event (Event ())
rbp = undefined

-- b1 untilB (lbp t0) ==> \e -> b2 untilB e -=> b3
-- b1 until left button pressed then b2 until left button is released then finally b3

predicate :: Behavior Bool -> Time -> Event ()
predicate b t = (undefined,())


-- choose the earlier of two events
(*|*) :: Event a -> Event a -> Event a
a *|* b = undefined


snapshot :: Event a -> Behavior b -> Event (a,b)
snapshot = undefined





main :: IO ()
main = return ()

