module Bananaconda where

import System.Random


type Load = Stack -> Maybe Stack
type Stack = [Either Stack_Bools_Ints String]
type Stack_Bools_Ints = Either Bool Int
 

data Cmd = PushS String -- Grace
         | PushB Bool -- Grace
         | Add -- Soren
         | Drop -- Grace
         | Equ -- Grace
         | IfElse Stack Stack -- Brian
         | Size_of_stack -- Reed
         | Error -- Brian
         | Randverb
  deriving (Eq,Show)
verblist = ["chase", "question", "reach", "kick", "yell"]
--




cmd :: Cmd -> Load
cmd Add         = \x -> case x of
                           (Right i : Right j : x') -> Just (Right (j ++ " " ++ i ) : x')
                           _ -> Nothing    

cmd Randverb    = \x -> case x of
                           (Right i : Right j : x') -> Just (Right (randverb verblist) : x')
                           _ -> Nothing 

--make_IO :: [String] -> IO [String]



randverbsec :: Int -> String
randverbsec _ = "Work"


randverb ::  [a] -> IO a
randverb q = do
         z <- randomRIO (1, 15) :: IO Int
         return (q !! z)
--randverb IO 2 = "chase"



