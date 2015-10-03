{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE InstanceSigs #-}


import Network.HTTP
--import Network.HTTP.Conduit
import Control.Concurrent.Async
import Text.Printf
import Control.Exception
import System.CPUTime


-- loop :: Int -> IO ()
-- loop i = 	
-- 	if i == 0 then do
-- 		putStrLn "ALL DONE!!!"
-- 		return ()
-- 	else do
-- 		res <- simpleHTTP (getRequest "http://www.jet.com/")
-- 		body <- getResponseBody res
-- 		putStrLn body
-- 		loop (i - 1)



loop :: String -> Int -> [IO ()]
loop url i = go i [] where
    go i xs = case i of
        0 -> xs
        i -> go (i - 1) ((simpleHTTP (getRequest url) >>= getResponseBody >>= putStrLn):xs)


main::IO ()
main = do
    putStrLn "Starting"
    let url = "http://jet.com/"
    start <- getCPUTime
    let x = loop url 10
    x <- mapConcurrently id x
    end <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    putStrLn (printf "Computation time: %0.3f sec\n" (diff :: Double))
    putStrLn "Done"
    return ()
    --loop 1
    --res <- simpleHTTP (getRequest "http://www.jet.com/")
    --body <- getResponseBody res
    --putStrLn body