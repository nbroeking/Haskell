main::IO()
main = let answer = factorial 20
       in
           print answer


factorial::Integer -> Integer
factorial x = 
            if x == 0
                then 1
            else 
                  x * factorial (x-1) 
