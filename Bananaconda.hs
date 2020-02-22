module Bananaconda where



data Stack = PushS String
         | PushB Bool
         | Add
         | Drop
         | Equ
         | IfElse Prog Prog
         | Size_of_stack
         | Error
  deriving (Eq,Show)