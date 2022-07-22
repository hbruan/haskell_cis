type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi x a b c
  | x <= 0 = []
  | x == 1 = [(a, b)]
  | otherwise = (hanoi (x-1) a c b) ++ [(a, b)] ++ (hanoi (x-1) c b a)

a d
a b
a c
a b