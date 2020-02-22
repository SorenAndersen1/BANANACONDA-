module Bananaconda where

type Stack = [Cmd]

data Cmd = PushS String
         | PushB Bool
         | Add
         | Drop
         | Equ
         | IfElse Prog Prog
         | Size_of_stack
         | Error
  deriving (Eq,Show)