{-# LANGUAGE BangPatterns  #-}
import Text.Printf
import System.CPUTime

time :: IO t -> IO t
time f = do
    start <- getCPUTime --pico seconds
    v <- f
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "ET: %0.3f sec\n" (diff::Double)
    return v

time_test = do
    -- lazyness must be understood to use this effectively
    putStrLn "TIME TEST"
    time $ product [1..10000] `seq` return ()

main = do
    time_test
