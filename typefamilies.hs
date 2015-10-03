{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

type family XList a

--data instance XList Char = XCons !Char !(XList Char) | XNil

data instance XList () = XListUnit !Int


main :: IO ()
main = do
	putStrLn "hello world"