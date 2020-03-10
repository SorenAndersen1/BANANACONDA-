module Bananaconda where
import Prelude hiding (Drop)


   

type Prog = [Cmd]


type Load = Stack -> Maybe Stack

type Stack = [Either Int String]

data Cmd = PushS String -- Grace
         | PushI Int -- Grace
         | Add -- Soren
         | Drop -- Grace
         | Equ -- Grace
         | IfElse Prog Prog -- Brian
         | Size_of_stack -- Reed
         | Error -- Brian
         | Randverb Int
         | Randnoun Int
         | Randadj  Int
  deriving (Eq,Show)
ex1 :: Stack
ex1 = [Right  "what", Left 1, Right "woo"]  
ex2 :: Prog
ex2 = [Randadj 2, Randnoun 4, Add]
verblist = ["chase", "question", "reach", "kick", "yell"]
nounlist = ["car", "fire extinguisher", "ball", "pool", "tree", "house", "dog", "snake", "computer", "phone", "road", "light", "cave", "baby", "camper"]
adjectivelist = ["jumpy", "slimy", "moist", "cold", "hot", "bright", "hairy", "sticky", "loud", "colorful", "comfy", "soft", "hard", "lumpy", "long"]

size_of_stack :: Stack -> Int
size_of_stack []      = 0
size_of_stack (x:xs)  = 1 + size_of_stack xs


cmd :: Cmd -> Load
cmd Add         = \x -> case x of
                           (Right i : Right j : x') -> Just (Right (j ++ " " ++ i ) : x')
                           (Left i : Left j : x') -> Just (Left (j + i ) : x')
                           _ -> Nothing

cmd (PushS s) = \x -> Just (Right s : x)
cmd (PushI i) = \x -> Just (Left i : x)


cmd (Randverb y)   =  \x -> Just (Right  (randword verblist y) : x)
                           
cmd (Randnoun y)   = \x ->  Just (Right  (randword nounlist y) : x)
                           
cmd (Randadj y)   = \x ->  Just (Right  (randword adjectivelist y) : x)
                           
cmd (IfElse s ss) = \x -> case x of
                        (Left 1 : x') -> prog s x'   --true
                        (Left 0 : x') -> prog ss x'  --false
                        _ -> Nothing
                        



randword :: [String] -> Int -> String
randword [] _ = " "
randword x y = x !! y

prog :: Prog -> Load
prog []       = \s -> Just s               -- when empty stack return stack
prog (c:p)    = \s -> case cmd c s of
                    Just s' -> prog p s'    --if cmd c s succeeds it returns a Just s', -> recursive call to rest of stack
                    _ -> Nothing

drop_stack :: Stack -> Stack
drop_stack [] = [Left 0] --might have to change to error (underflow)
drop_stack (x : stack) = stack

swapStack :: Stack -> Stack
swapStack [] = [Left 0] --change to error
swapStack (a : b : stack) = (b : a : stack)

dupStack :: Stack -> Stack
dupStack [] = [Left 0]
dupStack (x : stack) = (x : x : stack)

swapDropStack :: Stack -> Int -> Stack
swapDropStack [] x = [Left 0]
swapDropStack (x : stack) y = (stack !! y : stack)




run :: Prog -> Maybe Stack
run p = prog p []
