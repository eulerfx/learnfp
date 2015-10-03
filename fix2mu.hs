{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE InstanceSigs #-}

--newtype Fix f = Fix (f (Fix f))

newtype Fix f = Fix { unFix :: f (Fix f) }

cata :: Functor f => (f a -> a) -> (Fix f -> a)
cata f = f . fmap (cata f) . unFix

ana :: Functor f => (a -> f a) -> (a -> Fix f)
ana f = Fix . fmap (ana f) . f


newtype Mu f = Mu { runMu :: forall a. (f a -> a) -> a }

fixToMu :: Functor f => Fix f -> Mu f
fixToMu (Fix expr) =
    Mu $ \f -> f . fmap (($f) . runMu . fixToMu) $ expr



data L a b = Nil | Cons a b

type List a = Fix (L a)


instance Functor (L a) where
    fmap :: (b -> c) -> (L a b) -> (L a c)
    fmap f x = case x of
        Nil      -> Nil
        Cons a b -> Cons a (f b)


--unit :: a -> List a
--unit a = Fix . (Cons Nil a)
--unit = ana $ \x -> Cons (_) x
--unit = ana $ \a -> Cons a Nil

length :: List a -> Int
length = cata $ \x -> case x of
    Nil      -> 0
    Cons _ n -> n + 1

sum :: Num a => List a -> a
sum = cata $ \x -> case x of
    Nil      -> 0
    Cons a s -> a + s    



main::IO ()
main = do
    putStrLn "hello world 2"