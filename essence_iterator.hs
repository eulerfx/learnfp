{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}

-- https://www.cs.ox.ac.uk/jeremy.gibbons/publications/iterator.pdf

module Iterator where

import Data.Bifunctor
import Data.Monoid
import Data.Foldable
import Control.Applicative hiding (Const)
import Control.Comonad
import Control.Monad
import Data.Char

data Fix s a = In { out :: s a (Fix s a) }

bmap :: Bifunctor s => (a -> b) -> Fix s a -> Fix s b
bmap f = In . bimap f (bmap f) . out

bfold :: Bifunctor s => (s a b -> b) -> Fix s a -> b
bfold f = f . bimap id (bfold f) . out

bunfold :: Bifunctor s => (b -> s a b) -> b -> Fix s a
bunfold f = In . bimap id (bunfold f) . f




data Stream a = SCons a (Stream a)

instance Functor Stream where
	fmap f (SCons x xs) = SCons (f x) (fmap f xs)

instance Applicative Stream where
	pure a = SCons a (pure a)
	SCons f fs <*> SCons x xs = SCons (f x) (fs <*> xs)

instance Comonad Stream where
	extract (SCons a _) = a
	duplicate (SCons a tl) = SCons (SCons a tl) (cojoin tl)

-- instance Monad Stream where
-- 	return a = SCons a (return a)
-- 	(SCons a tl) >>= f = SCons (f a) (tl >>= f)

counit :: Stream a -> a
counit (SCons a _) = a

cojoin :: Stream a -> Stream (Stream a)
cojoin (SCons a tl) = SCons (SCons a tl) (cojoin tl)

srepeat :: a -> Stream a
srepeat a = SCons a (srepeat a)







newtype Reader r a = Reader { unReader :: r -> a }

ask :: Reader r r
ask = Reader id




instance Functor (Reader r) where
	fmap f ra = Reader (\r -> f ((unReader ra) r))

instance Applicative (Reader r) where
	-- pure :: a -> Reader r a
	pure = Reader . const
	(Reader f) <*> (Reader a) = Reader $ \r -> (f r) (a r)

instance Monad (Reader r) where
	return a = Reader $ \r -> a
	(Reader fra) >>= f = Reader $ \r -> 
		let a = fra r in
		let (Reader frb) = f a in 
		frb r


newtype Const b a = Const { unConst :: b }

instance Monoid m => Functor (Const m) where
	fmap :: (a -> b) -> (Const m a) -> (Const m b)
	fmap f = Const . unConst

instance Monoid b => Applicative (Const b) where
	pure _ = Const mempty
	(Const x) <*> (Const y) = Const (x `mappend` y)







			
-- instance Applicative [] where
-- 	pure x = xs where xs = x:xs
-- 	(f:fs) <*> (x:xs) = f x:(fs <*> xs)
-- 	_ <*> _ = []

-- type Prod<'F, 'G, 'a> = { fst : App<'F, 'a> ; snd : App<'G, 'a> }

data (m • n) a = Prod { pfst :: m a, psnd :: n a }

instance (Functor m, Functor n) => Functor (m • n) where
	fmap f mn = Prod (fmap f (pfst mn)) (fmap f (psnd mn))

instance (Applicative m, Applicative n) => Applicative (m • n) where
	pure x = Prod (pure x) (pure x)
	mf <*> mx = Prod (pfst mf <*> pfst mx) (psnd mf <*> psnd mx)


-- (°) :: (Functor m, Functor n) => (a -> m b) -> (a -> n b) -> (a -> (m • n) b)
-- (f ° g) x = Prod (f x) (g x)

(•) :: (Functor m, Functor n) => (a -> m b) -> (a -> n b) -> (a -> (m • n) b)
(f • g) x = Prod (f x) (g x)


data (m ° n) a = Comp { unComp :: m (n a) }

instance (Functor m, Functor n) => Functor (m ° n) where
	fmap :: (a -> b) -> (m ° n) a -> (m ° n) b
	fmap f mn = Comp $ fmap (fmap f) (unComp mn)

instance (Applicative m, Applicative n) => Applicative (m ° n) where
	pure x = Comp $ pure (pure x)
	-- (<*>) :: (m ° n) (a -> b) -> (m ° n) a -> (m ° n) b
	-- pure (<*>) :: m (n (a -> b) -> n a -> n b)
	-- pure (<*>) <*> fs :: m (n a -> n b)
	-- pure (<*>) <*> fs <*> xs :: m (n b)
	(Comp fs) <*> (Comp xs) = Comp $ pure (<*>) <*> fs <*> xs

(°) :: (Functor m, Functor n) => (b -> n c) -> (a -> m b) -> (a -> (m ° n) c)
f ° g = Comp . fmap f . g






traverseList :: (Applicative m) => (a -> m b) -> [a] -> m [b]
traverseList f [] = pure []
traverseList f (x:xs) = pure (:) <*> f x <*> traverseList f xs

distList :: (Applicative m) => [m a] -> m [a]
distList = traverseList id



class Functor t => Traversable t where
	traverse :: Applicative m => (a -> m b) -> t a -> m (t b)
	traverse f = dist . fmap f
	dist :: Applicative m => t (m a) -> m (t a)
	dist = traverse id





data Tree a = Leaf a | Bin (Tree a) (Tree a) deriving (Show)

instance Functor Tree where
	fmap f (Leaf a) = Leaf (f a)
	fmap f (Bin l r) = Bin (fmap f l) (fmap f r)

instance Foldable Tree where
	foldMap f (Leaf a) = mappend (f a) mempty
	foldMap f (Bin l r) = mappend (foldMap f l) (foldMap f r)

instance Traversable Tree where
	traverse f (Leaf a) = pure Leaf <*> (f a)  
	traverse f (Bin l r) = pure Bin <*> traverse f l <*> traverse f r		


class Bifunctor s => Bitraversable s where
	bidist :: Applicative m => s (m a) (m b) -> m (s a b)	


instance Bitraversable s => Functor (Fix s) where
	fmap f = bmap f

instance Bitraversable s => Traversable (Fix s) where
	traverse :: Applicative m => (a -> m b) -> Fix s a -> m (Fix s b)
	traverse f = bfold (fmap In . bidist . bimap f id)


newtype Id a = Id { unId :: a }

instance Functor Id where
	fmap f = Id . f . unId

instance Applicative Id where
	pure a = Id a
	(Id f) <*> (Id a) = Id (f a)


reduce :: (Traversable t, Monoid m) => (a -> m) -> t a -> m
reduce f = unConst . traverse (Const . f)

crush :: (Traversable t, Monoid m) => t m -> m
crush = reduce id

-- tsum :: (Traversable t) => t Integer -> Integer
-- tsum = crush






class Coerce a b | a -> b where
	cdown :: a -> b
	cup :: b -> a

instance Coerce (Id a) a where
	cdown = unId
	cup = Id

instance Coerce (Const a b) a where
	cdown = unConst
	cup = Const

instance (Coerce (m a) b, Coerce (n a) c) => Coerce ((m • n) a) (b,c) where
	cdown mnx = (cdown (pfst mnx),cdown (psnd mnx))
	cup (x,y) = Prod (cup x) (cup y)

instance (Functor m, Functor n, Coerce (m b) c, Coerce (n a) b) => Coerce ((m ° n) a) c where
	cdown = cdown . fmap cdown . unComp
	cup = Comp . fmap cup . cup 



contentsBody :: a -> Const [a] b
contentsBody a = cup [a]

contents :: Traversable t => t a -> Const [a] (t b)
contents = traverse contentsBody	

run :: (Coerce b c, Traversable t) => (t a -> b) -> t a -> c
run program = cdown . program

runContents :: Traversable t => t a -> [a]
runContents = run contents


shapeBody :: a -> Id ()
shapeBody _ = cup ()

shape :: Traversable t => t a -> Id (t ())
shape = traverse shapeBody


decompose :: Traversable t => t a -> (Id • Const [a]) (t ())
-- decompose = shape ° contents
decompose = traverse (shapeBody • contentsBody)


instance Coerce (Maybe a) (Maybe a) where
	cdown = id
	cup = id



newtype State s a = State { runState :: s -> (a,s) }

get :: State s s
get = State $ \s -> (s,s)

put :: s -> State s ()
put a = State $ \s -> ((),a) 

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)


instance Functor (State s) where
	fmap f (State st) = State $ \s -> let (a,s') = st s in (f a, s')

instance Applicative (State s) where
	pure a = State $ \s -> (a,s)
	(State sab) <*> (State sa) = State $ \s -> 
		let (ab,s') = sab s in
		let (a,s'') = sa s' in
		(ab a,s'')

instance Monad (State s) where
	return a = State $ \s -> (a,s)
	(State sa) >>= f = State $ \s ->
		let (a,s') = sa s in
		let (b,s'') = (runState (f a)) s' in
		(b,s'')		

instance Coerce (State s a) (s -> (a,s)) where
	cdown = runState
	cup = State


reassembleBody :: () -> (State [a] ° Maybe) a
reassembleBody = cup . weakHead where
	weakHead _ [] = (Nothing, [])
	weakHead _ (y:ys) = (Just y, ys)

reassemble :: Traversable t => t () -> (State [a] ° Maybe) (t a)
reassemble = traverse reassembleBody

runReassemble :: Traversable t => (t (), [a]) -> Maybe (t a)
runReassemble = fst . uncurry (run reassemble)



-- runDecompose xs = (ys,zs) =
-- 	fmap (curry runReassemble ys) (traverseList fs zs) = fmap (Just) (traverse f xs)


collect :: (Traversable t, Applicative m) => (a -> m ()) -> (a -> b) -> t a -> m (t b)
collect f g = traverse (\a -> pure (\() -> g a) <*> f a)

loop :: (Traversable t) => (a -> b) -> t a -> State Integer (t b)
loop touch = collect (\a -> do { n <- get ; put (n + 1) }) touch


disperse :: (Traversable t, Applicative m) => m b -> (a -> b -> c) -> t a -> m (t c)
disperse mb g = traverse (\a -> pure (g a) <*> mb)


label :: Traversable t => t a -> State Integer (t Integer)
label = disperse step (curry snd)

step :: State Integer Integer
step = do
	n <- get
	put (n + 1)
	return n




newtype Backwards m a = Backwards { runBackwards :: m a }

instance Functor m => Functor (Backwards m) where
	fmap f = Backwards . fmap f . runBackwards

instance Applicative m => Applicative (Backwards m) where
	pure = Backwards . pure
	(Backwards f) <*> (Backwards a) = Backwards $ pure (flip ($)) <*> a <*> f


data AppAdapter m where
	AppAdapter :: Applicative (g m) => 
		(forall a. m a -> g m a) -> (forall a. g m a -> m a) -> AppAdapter m

backwards :: Applicative m => AppAdapter m
backwards = AppAdapter Backwards runBackwards

ptraverse :: (Applicative m, Traversable t) => AppAdapter m -> (a -> m b) -> t a -> m (t b)
ptraverse (AppAdapter insert retrieve) f = retrieve . traverse (insert . f)

lebal :: Traversable t => t a -> State Integer (t Integer)
lebal = ptraverse backwards (\a -> step)



-- distLaw :: (Traversable t, Applicative m) => (a -> b) -> t (m a) -> Bool
-- distLaw f tma = dist . fmap (fmap f) == fmap (fmap f) . dist

-- dist . fmap Id = Id
-- dist . fmap Comp = Comp . fmap dist . dist


(<<*>>) :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
g <<*>> f = \a -> f a >>= g

-- (mx >>= \x -> my >>= \y -> (x,y)) = (my >>= \y -> mx >>= \x -> (x,y))



update1 :: a -> State Integer a
update1 x = do { var <- get ; put (var * 2) ; return x }

update2 :: a -> State Integer a
update2 x = do { var <- get ; put (var + 1); return x }

-- monadic1 = traverse update1 <<*>> traverse update2
-- monadic2 = traverse (update1 <<*>> update2)


instance Traversable [] where
	traverse f [] = pure []
	--traverse f (x:xs) = pure (const (:)) <*> f x <*> traverse f xs
	traverse f (x:xs) = pure (:) <*> f x <*> traverse f xs



type Count = Const Integer

instance Monoid Integer where
	mempty = 0
	mappend = (+)

count :: a -> Count b
count _ = Const 1

cciBody :: Char -> Count a
cciBody = count

cci :: String -> Count [a]
cci = traverse cciBody


test :: Bool -> Integer
test b = if b then 1 else 0

lciBody::Char -> Count a
lciBody c = cup (test (c == '\n'))

lci :: String -> Count [a]
lci = traverse lciBody


wciBody :: Char -> ((State Bool) ° Count) a
wciBody c = cup (updateState c) where
	updateState :: Char -> Bool -> (Integer,Bool)
	updateState c w = let s = not (isSpace c) in (test (not w && s),s)

wci :: String -> ((State Bool) ° Count) [a]
wci = traverse wciBody


clci :: String -> (Count • Count) [a]
-- clci = cci ° lci
clci = traverse (cciBody • lciBody)


-- (m ° n) a :: m (n a) 

clwci :: String -> ((Count • Count) • ((State Bool) ° Count)) [a]
clwci = traverse (cciBody • lciBody • wciBody)


newtype Pair a = P (a,a)

instance Functor Pair where
	fmap f (P (a,b)) = P (f a, f b)

instance Applicative Pair where
	pure a = P (a,a)
	P (f,g) <*> P (x,y) = P (f x, g y)

quiBody :: Char -> Pair Bool
quiBody c = P (c == 'q', c == 'u')

qui :: String -> Pair [Bool]
qui = traverse quiBody


-- ⊠ = Prod = •
-- 􏰂􏰂  = Comp = °
-- ⊗ = ° = (a -> m b) ° (a -> n b) = (a -> (m ° n) b)
-- ⊙􏰂 =   = (a -> m b)   (a -> n b) = (a -> (m ))

ccqui :: String -> (Count • Pair) [Bool]
ccqui = traverse (cciBody • quiBody)


wcqui :: String -> (Pair • (State Bool ° Count)) [Bool]
wcqui = qui • wci

wcqui' :: String -> ((Id • (State Bool ° Count)) ° Pair) [Bool]
wcqui' = traverse (quiBody ° (Id • wciBody))



newtype Writer w a = Writer { unWriter :: (a,w) }

instance Monoid w => Functor (Writer w) where
	fmap f (Writer (a,w)) = Writer (f a, w)

instance Monoid w => Applicative (Writer w) where
	pure a = Writer (a, mempty)
	(Writer (f,w)) <*> (Writer (a,w')) = Writer (f a, mappend w w')

instance Monoid w => Monad (Writer w) where
	return a = Writer (a,mempty)
	(Writer (a,w)) >>= f = let Writer (b,w') = f a in Writer (b, mappend w w')

tell :: Monoid w => w -> Writer w ()
tell w = Writer ((), w)

listen :: Monoid w => a -> Writer w a
listen a = Writer (a, mempty)

--pass :: Monoid w => Writer (w -> w) a -> a



ccmBody :: Char -> Writer Integer Char
ccmBody c = do { tell 1 ; return c }

ccm :: String -> Writer Integer String
ccm = mapM ccmBody

lcmBody :: Char -> Writer Integer Char
lcmBody c = do { tell (test (c == '\n')) ; return c }

lcm :: String -> Writer Integer String
lcm = mapM lcmBody


wcmBody :: Char -> State (Integer,Bool) Char
wcmBody c = let s = not (isSpace c) in do
	(n,w) <- get
	put (n + test (not w && s),s)
	return c

wcm :: String -> State (Integer,Bool) String
wcm = mapM wcmBody


-- clwcm = ccm • lcm • wcm
-- clwcm = mapM (ccmBody • lcmBody • wcmBody)


qumBody :: Char -> Reader Bool Bool
qumBody c = do { b <- ask ; return (if b then (c == 'q') else (c == 'u')) }

qum :: String -> Reader Bool [Bool]
qum = mapM qumBody



class MonadTrans t where
	lift :: Monad m => m a -> t m a

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

instance Monad m => Monad (StateT s m) where
	return a = StateT $ \s -> return (a,s)
	(StateT st) >>= f = StateT $ \s -> do
		(a,s) <- st s
		let (StateT st') = f a
		(a,s') <- st' s
		return (a,s')

instance Monad m => Functor (StateT s m) where
	fmap f (StateT st) = StateT $ \s -> do
		(a,s') <- st s
		return (f a,s')

instance Monad m => Applicative (StateT s m) where
	pure a = StateT $ \s -> return (a,s)
	(StateT f) <*> (StateT a) = StateT $ \s -> do
		(f,s') <- f s
		(a,s'') <- a s'
		return (f a,s'')

instance MonadTrans (StateT s) where
	lift :: Monad m => m a -> StateT s m a
	lift ma = StateT $ \s -> do
		a <- ma
		return (a,s)

class Monad m => MonadState s m | m -> s where
	getT :: m s
	putT :: s -> m ()

instance MonadState s (State s) where
	getT = State $ \s -> (s,s)
	putT s = State $ \_ -> ((),s)

instance Monad m => MonadState s (StateT s m) where
	getT = StateT $ \s -> return (s,s)
	putT s = StateT $ \_ -> return ((),s)	


(‡) :: (Monad m, MonadTrans t, Monad (t m)) => (b -> t m c) -> (a -> m b) -> (a -> t m c)
p1 ‡ p2 = p1 <<*>> (lift . p2)

(€) :: (Monad m, MonadTrans t, Monad (t m)) => (b -> m c) -> (a -> t m b) -> (a -> t m c)
p1 € p2 = (lift . p1) <<*>> p2



-- wcmBody' :: MonadState (Integer, Bool) m => Char -> m Char
-- wcmBody' c = let s = not (isSpace c) in do
-- 	(n,w) <- get
-- 	put (n + test (not w && s),s)
-- 	return c

-- quwcm :: String -> StateT (Integer,Bool) (Reader Bool) [Bool]
-- quwcm = mapM qumBody € mapM wcmBody'




main :: IO ()
main = do	
	let t = Bin (Leaf 1) (Leaf 2)
	let x = loop (\a -> a + 2) t
	let (t,s) = (runState x) 1
	putStrLn (show t)
	putStrLn (show s)
	return ()






