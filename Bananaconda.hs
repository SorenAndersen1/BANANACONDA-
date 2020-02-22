module Bananaconda where

type Stack = [Cmd]

data Cmd = PushS String
         | PushB Bool
         | Add
         | Drop
         | Equ
         | IfElse Stack Stack
         | Size_of_stack
         | Error
  deriving (Eq,Show)
