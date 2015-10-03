{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE InstanceSigs #-}



newtype Fix f = Fix { unFix :: f (Fix f) }

cata :: Functor f => (f a -> a) -> (Fix f -> a)
cata f = f . fmap (cata f) . unFix

ana :: Functor f => (a -> f a) -> (a -> Fix f)
ana f = Fix . fmap (ana f) . f

---

data L a b = Nil | Cons a b

type List a = Fix (L a)


instance Functor (L a) where
    --fmap :: (b -> c) -> (L a b) -> (L a c)
    fmap f x = case x of
        Nil      -> Nil
        Cons a b -> Cons a (f b)


empty :: List a
empty = Fix Nil

cons :: a -> List a -> List a
cons a xs = Fix (Cons a xs)

unit :: a -> List a
unit a = cons a empty


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
    putStrLn "hello world"