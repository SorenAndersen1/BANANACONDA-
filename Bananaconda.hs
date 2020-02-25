module Bananaconda where



type Load = Stack -> Maybe Stack
type Stack = [Either (Bool,Int) String]



data Cmd = PushS String -- Grace
         | PushB Bool -- Grace
         | Add -- Soren
         | Drop -- Grace
         | Equ -- Grace
         | IfElse Stack Stack -- Brian
         | Size_of_stack -- Reed
         | Error -- Brian
         | Randverb Int
  deriving (Eq,Show)
verblist = ["chase", "question", "reach", "kick", "yell"]
nounlist = ["car", "fire extinguisher", "ball", "pool", "tree", "house", "dog", "snake", "computer", "phone", "road", "light", "cave", "baby", "camper"]

Size_of_stack :: Stack -> Int
Size_of_stack []      = 0
Size_of_stack (x:xs)  = 1 + Size_of_stack xs

cmd :: Cmd -> Load
cmd Add         = \x -> case x of
                           (Right i : Right j : x') -> Just (Right (j ++ " " ++ i ) : x')
                           _ -> Nothing

cmd (Randverb y)   = \x -> case x of
                           (Right i : Right j : x') -> Just (Right (randverb verblist y) : x')
                           _ -> Nothing

randverb :: [String] -> Int -> String
randverb [] _ = " "
randverb x y = x !! y

--make_IO :: [String] -> IO [String]
