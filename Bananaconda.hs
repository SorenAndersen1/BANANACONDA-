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


data Type = TBool | TInt | TError | TString
  deriving (Eq, Show)
  

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


-- Typing Relation
typeOf :: cmd -> Type
typeOf (PushS s)     = TString
typeOf (PushI i)     = TInt
typeOf (Add s)       = case (typeOf s) of
                        (TString)  -> TString
                        _          -> TError
typeOf (Randverb y)  = case (typeOf y) of
                        (TString)  -> TString
                        _           -> TError
typeOf (Randnoun y)  = case (typeOf y) of
                        (TString)  -> TString
                        _           -> TError
typeOf (Randadj y)  = case (typeOf y) of
                        (TString)  -> TString
                        _           -> TError
typeOf (IfElse s ss) = case (typeOf s, typeOf ss) of
                        (ts, tss) -> if ts == tss then ts else TError
                        _         -> TError



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

getBottom :: Stack -> Either Int String
--getBottom []  = error
getBottom [Left i] = Left i
getBottom [Right s] = Right s
getBottom (_:xs) = getBottom xs



run :: Prog -> Maybe Stack
run p = prog p []

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = f [] xs xs
  where
    f ss xs []              = ss == xs
    f ss (_:xs) [_]         = ss == xs
    f ss (x:xs) (_:_:es)    = f (x:ss) xs es
    
ex3 = [PushI 0, IfElse [PushS "5", PushS "6", Add] [PushS "11"]]
ex4 = [Left 1, Left 2]


