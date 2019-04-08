module Line where

import           Term

type Output = String

line :: Term -> Int -> Output
line t a = "eval (" ++ show t ++ ") ⇐ " ++ show a ++ "\n"
