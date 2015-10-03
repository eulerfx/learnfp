
-- http://web.cecs.pdx.edu/~mpj/pubs/springschool95.pdf

import Prelude hiding (length)

not :: Bool -> Bool
not False = True
not True = False

identity :: a -> a
identity a = a

length :: [a] -> Int
length [] = 0
length (x:xs) = 1 + length xs




main :: IO ()
main = return ()