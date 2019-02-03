module Dir where
data Dir = U | D | L | R deriving(Show)

apply :: Dir -> (Integer, Integer) -> (Integer, Integer)
apply U (a, b) = (a, b-1)
apply D (a, b) = (a, b+1)
apply L (a, b) = (a-1, b)
apply R (a, b) = (a+1, b)
