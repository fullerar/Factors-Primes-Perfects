-- Andrew Fuller
-- JMU Spring '16
-- Factors, Primes, Perfects



-- Generate a list of all factors of n
factors :: Integral a => a -> [a]
factors n = [x | x <- [1..n], n `mod` x == 0]


 
-- True iff n is prime
isPrime :: Integral a => a -> Bool
isPrime n = n > 1 && null[x | x <- [2..n-1], n `mod` x == 0]



-- Generate a list of all prime factors of n
primeFactors :: Integral a => a -> [a]
primeFactors n = [x | x <- [1..n], n `mod` x == 0, isPrime x]



-- Generate a list of primes up to n
primesUpTo :: Integral a => a -> [a]
primesUpTo n = [x | x <- [1..n], isPrime x]



-- True iff n is a perfect number
-- A number n is perfect if the sum of its factors is 2*n
isPerfect :: Integral a => a -> Bool
isPerfect n = n > 0 && sum (factors n) == 2 * n



-- Generate a list of all perfect numbers up to n
perfectUpTo :: Integral a => a -> [a]
perfectUpTo n = [x | x <- [1..n], isPerfect x]



-- Generate the next prime greater than n
nextPrime :: Integral a => a -> a
nextPrime n | isPrime next = next
            | otherwise = nextPrime next
        where
            next = n + 1



-- Generate the first n primes
generatePrimes :: Integral a => a -> [a]
generatePrimes n = primeAccum n 0

-- Helper function for the generatePrimes function
primeAccum primesLeft currentPrime
 | (primesLeft <= 0) = []
 | otherwise = (nextPrime currentPrime):primeAccum (primesLeft-1) (nextPrime currentPrime) 
