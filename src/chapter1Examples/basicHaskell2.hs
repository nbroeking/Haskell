main :: IO ()
main =
    let list = [1, 3, 8, 11, 13]
        loop [] = void
        loop (x:xs) = print x >> loop xs
        in
        loop list
