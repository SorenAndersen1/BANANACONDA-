module Bananaconda where



type Load = Stack -> Maybe Stack
type Stack = [Either (Bool,Int) String]
type Prog = [Cmd]



data Cmd = PushS String -- Grace
         | PushB Bool -- Grace
         | Add -- Soren
         | Drop -- Grace
         | Equ -- Grace
         | IfElse Prog Prog -- Brian
         | Size_of_stack -- Reed
         | Error -- Brian
         | Randverb Int
  deriving (Eq,Show)
verblist = ["chase", "question", "reach", "kick", "yell"]
nounlist = ["car", "fire extinguisher", "ball", "pool", "tree", "house", "dog", "snake", "computer", "phone", "road", "light", "cave", "baby", "camper"]

--Size_of_stack :: Stack -> Int
--Size_of_stack []      = 0
--Size_of_stack (x:xs)  = 1 + Size_of_stack xs

cmd :: Cmd -> Load
cmd Add         = \x -> case x of
                           (Right i : Right j : x') -> Just (Right (j ++ " " ++ i ) : x')
                           _ -> Nothing

cmd (Randverb y)   = \x -> case x of
                           (Right i : Right j : x') -> Just (Right (randverb verblist y) : x')
                           _ -> Nothing

cmd (IfElse s ss) = \x -> case x of
                        (Left (True, _)  : x') -> prog s x'
                        (Left (False, _) : x') -> prog ss x'
                        _ -> Nothing


randverb :: [String] -> Int -> String
randverb [] _ = " "
randverb x y = x !! y

--make_IO :: [String] -> IO [String]



prog :: Prog -> Load
prog []       = \s -> Just s               -- when empty stack return stack
prog (c:p)    = \s -> case cmd c s of
                    Just s' -> prog p s'    --if cmd c s succeeds it returns a Just s', -> recursive call to rest of stack
                    _ -> Nothing

ex2 = [PushB True, IfElse [PushS "5", PushS "6", Add] [PushS "11"]]
