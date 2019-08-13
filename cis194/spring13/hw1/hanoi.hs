type Peg = String
type Move = (Peg, Peg)


-- https://en.wikipedia.org/wiki/Tower_of_Hanoi#Recursive_solution
-- Move m-1 disks from source to spare
-- Move 1 disks from source to target
-- Move m-1 disk from spare to target
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n src dst spare
  | n == 0 = []
  | n == 1 = [(src, dst)]
  | otherwise = hanoi (n-1) src spare dst ++
    hanoi 1 src dst spare ++
    hanoi (n-1) spare dst src


main :: IO ()
main = do
  putStrLn "Towers of Hanoi"
  putStrLn . show $ hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]
