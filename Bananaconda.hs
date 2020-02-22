module Bananaconda where

type Stack = [Cmd]

data Cmd = PushS String -- Grace
         | PushB Bool -- Grace
         | Add -- Soren
         | Drop -- Grace
         | Equ -- Grace
         | IfElse Prog Prog -- Brian
         | Size_of_stack -- Reed
         | Error -- Brian
  deriving (Eq,Show)