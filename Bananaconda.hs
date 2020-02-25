module Bananaconda where



type Load = Stack -> Maybe Stack
type Stack = [Either Int String]



data Cmd = PushS String -- Grace
         | PushB Bool -- Grace
         | Add -- Soren
         | Drop -- Grace
         | Equ -- Grace
         | IfElse Stack Stack -- Brian
         | Error -- Brian
         | Randverb Int
         | Randnoun Int
         | Randadj  Int
  deriving (Eq,Show)
verblist = ["chase", "question", "reach", "kick", "yell"]
nounlist = ["car", "fire extinguisher", "ball", "pool", "tree", "house", "dog", "snake", "computer", "phone", "road", "light", "cave", "baby", "camper"]
adjectivelist = ["jumpy", "slimy", "moist", "cold", "hot", "bright", "hairy", "sticky", "loud", "colorful", "comfy", "soft", "hard", "lumpy", "long"]

size_of_stack :: Stack -> Int
size_of_stack []      = 0
size_of_stack (x:xs)  = 1 + size_of_stack xs

cmd :: Cmd -> Load
cmd Add         = \x -> case x of
                           (Right i : Right j : x') -> Just (Right (j ++ " " ++ i ) : x')
                           _ -> Nothing

cmd (Randverb y)   = \x -> case x of
                           (Right i : Right j : x') -> Just (Right (j ++ " " ++ (randword verblist y) ++ " " ++ i) : x')
                           _ -> Nothing
cmd (Randnoun y)   = \x -> case x of
                           (Right i : Right j : x') -> Just (Right (j ++ " " ++ (randword nounlist y) ++ " " ++ i) : x')
                           _ -> Nothing
cmd (Randadj y)   = \x -> case x of
                           (Right i : Right j : x') -> Just (Right (j ++ " " ++ (randword adjectivelist y) ++ " " ++ i) : x')
                           _ -> Nothing

randword :: [String] -> Int -> String
randword [] _ = " "
randword x y = x !! y

--make_IO :: [String] -> IO [String]
