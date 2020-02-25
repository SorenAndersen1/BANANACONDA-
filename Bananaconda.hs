module Bananaconda where



type Load = Stack -> Maybe Stack
type Stack = [Either Int String]



data Cmd = PushS String -- Grace
         | PushI Int -- Grace
         | Add -- Soren
         | Equ -- Grace
         | IfElse Stack Stack -- Brian
         | Size_of_stack -- Reed
         | Error -- Brian
         | Randverb Int
  deriving (Eq,Show)
verblist = ["chase", "question", "reach", "kick", "yell"]
nounlist = ["car", "fire extinguisher", "ball", "pool", "tree", "house", "dog", "snake", "computer", "phone", "road", "light", "cave", "baby", "camper"]

-- Size_of_stack :: Stack -> Int
-- Size_of_stack []      = 0
-- Size_of_stack (x:xs)  = 1 + Size_of_stack xs

cmd :: Cmd -> Load
cmd Add         = \x -> case x of
                           (Right i : Right j : x') -> Just (Right (j ++ " " ++ i ) : x')
                           _ -> Nothing

cmd (Randverb y)   = \x -> case x of
                           (Right i : Right j : x') -> Just (Right (randverb verblist y) : x')
                           _ -> Nothing
cmd (PushS s) = \x -> Just (Right s : x)
cmd (PushI i) = \x -> Just (Left i : x)
-- cmd (PushI i) = \x -> Just (Left (_, i) : x)

randverb :: [String] -> Int -> String
randverb [] _ = " "
randverb x y = x !! y

drop :: Stack -> Stack
drop [] = [Left 0] --might have to change to error (underflow)
drop (x : stack) = stack
