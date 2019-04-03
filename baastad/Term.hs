module Term where

data Term = Con Int
  | Div Term Term deriving Show

answer, error :: Term
answer = (Div (Div (Con 1972 ) (Con 2 )) (Con 23 ))
error = (Div (Con 1 ) (Con 0 ))
