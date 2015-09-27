main::IO()
main = let answer = factorial 20
       in
           print answer


factorial::Integer -> Integer
factorial 0 = 1
factorial x = x * factorial (x-1) 
