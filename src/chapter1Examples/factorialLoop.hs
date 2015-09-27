import Control.Monad

main::IO()
main = forever $ do  
    putStr "\nEnter Input:\n"  
    input <- getLine
    
    let x = read input:: Integer
    let answer = factorial x
    print "Answer: "
    print answer

factorial::Integer -> Integer
factorial x = 
            if x == 0
                then 1
            else 
                  x * factorial (x-1) 
