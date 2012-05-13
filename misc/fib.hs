fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)

fib 0 (x,y) = x+y
fib n (x,y) = fib (n-1) (y,x+y)

