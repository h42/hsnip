getSieve :: Int ->[Int]
getSieve n = x where
    m = ceiling $ sqrt $ fromIntegral n
    x = 1:[2,4..m]

primes :: Int -> [Int]
primes nn = 2 : filter (isPrime $ primes nn) [3,5..nn]  where
  isPrime (p:ps) n
	| mod n p == 0 = False
	| p*p > n      = True
	| otherwise    = isPrime ps n
  isPrime [] _ = False -- never used, but avoids compiler warning

