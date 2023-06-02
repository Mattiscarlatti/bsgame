module Evaluation where

import View

evalInput :: String -> Either String String
evalInput j = do 
    if (length j == 2) then if 
        (elem (head (j)) "ABCD") then if
            (elem (head (tail (j))) "12345") then Right j else Left "Invalid input."
        else Left "Invalid input."
    else Left "Invalid input."
 
evalInput2 :: String -> Either String String
evalInput2 n = do
    game <- return (mkGame) --not a state game yet
    case head (tail (n)) of
                            '1' -> case head (n) of 
                                   'A' -> if ((firstField (fifthRow (gBoard game))) == bb) || ((firstField (fifthRow (gBoard game))) == wb) || ((firstField (fifthRow (gBoard game))) == bw) || ((firstField (fifthRow (gBoard game))) == ww) then Right n else Left "Invalid input."
                                   'B' -> if ((secondField (fifthRow (gBoard game))) == bb) || ((secondField (fifthRow (gBoard game))) == wb) || ((secondField (fifthRow (gBoard game))) == bw) || ((secondField (fifthRow (gBoard game))) == ww) then Right n else Left "Invalid input."
                                   'C' -> if ((thirdField (fifthRow (gBoard game))) == bb) || ((thirdField (fifthRow (gBoard game))) == wb) || ((thirdField (fifthRow (gBoard game))) == bw) || ((thirdField (fifthRow (gBoard game))) == ww) then Right n else Left "Invalid input."
                                   'D' -> if ((fourthField (fifthRow (gBoard game))) == bb) || ((fourthField (fifthRow (gBoard game))) == wb) || ((fourthField (fifthRow (gBoard game))) == bw) || ((fourthField (fifthRow (gBoard game))) == ww) then Right n else Left "Invalid input."
                            '2' -> case head (n) of 
                                   'A' -> if ((firstField (fourthRow (gBoard game))) == bb) || ((firstField (fourthRow (gBoard game))) == wb) || ((firstField (fourthRow (gBoard game))) == bw) || ((firstField (fourthRow (gBoard game))) == ww) then Right n else Left "Invalid input."
                                   'B' -> if ((secondField (fourthRow (gBoard game))) == bb) || ((secondField (fourthRow (gBoard game))) == wb) || ((secondField (fourthRow (gBoard game))) == bw) || ((secondField (fourthRow (gBoard game))) == ww) then Right n else Left "Invalid input."
                                   'C' -> if ((thirdField (fourthRow (gBoard game))) == bb) || ((thirdField (fourthRow (gBoard game))) == wb) || ((thirdField (fourthRow (gBoard game))) == bw) || ((thirdField (fourthRow (gBoard game))) == ww) then Right n else Left "Invalid input."
                                   'D' -> if ((fourthField (fourthRow (gBoard game))) == bb) || ((fourthField (fourthRow (gBoard game))) == wb) || ((fourthField (fourthRow (gBoard game))) == bw) || ((fourthField (fourthRow (gBoard game))) == ww) then Right n else Left "Invalid input."
                            '3' -> case head (n) of 
                                   'A' -> if ((firstField (thirdRow (gBoard game))) == bb) || ((firstField (thirdRow (gBoard game))) == wb) || ((firstField (thirdRow (gBoard game))) == bw) || ((firstField (thirdRow (gBoard game))) == ww) then Right n else Left "Invalid input."
                                   'B' -> if ((secondField (thirdRow (gBoard game))) == bb) || ((secondField (thirdRow (gBoard game))) == wb) || ((secondField (thirdRow (gBoard game))) == bw) || ((secondField (thirdRow (gBoard game))) == ww) then Right n else Left "Invalid input."
                                   'C' -> if ((thirdField (thirdRow (gBoard game))) == bb) || ((thirdField (thirdRow (gBoard game))) == wb) || ((thirdField (thirdRow (gBoard game))) == bw) || ((thirdField (thirdRow (gBoard game))) == ww) then Right n else Left "Invalid input."
                                   'D' -> if ((fourthField (thirdRow (gBoard game))) == bb) || ((fourthField (thirdRow (gBoard game))) == wb) || ((fourthField (thirdRow (gBoard game))) == bw) || ((fourthField (thirdRow (gBoard game))) == ww) then Right n else Left "Invalid input."                                                                     
                            '4' -> case head (n) of 
                                   'A' -> if ((firstField (secondRow (gBoard game))) == bb) || ((firstField (secondRow (gBoard game))) == wb) || ((firstField (secondRow (gBoard game))) == bw) || ((firstField (secondRow (gBoard game))) == ww) then Right n else Left "Invalid input."
                                   'B' -> if ((secondField (secondRow (gBoard game))) == bb) || ((secondField (secondRow (gBoard game))) == wb) || ((secondField (secondRow (gBoard game))) == bw) || ((secondField (secondRow (gBoard game))) == ww) then Right n else Left "Invalid input."
                                   'C' -> if ((thirdField (secondRow (gBoard game))) == bb) || ((thirdField (secondRow (gBoard game))) == wb) || ((thirdField (secondRow (gBoard game))) == bw) || ((thirdField (secondRow (gBoard game))) == ww) then Right n else Left "Invalid input."
                                   'D' -> if ((fourthField (secondRow (gBoard game))) == bb) || ((fourthField (secondRow (gBoard game))) == wb) || ((fourthField (secondRow (gBoard game))) == bw) || ((fourthField (secondRow (gBoard game))) == ww) then Right n else Left "Invalid input."
                            '5' -> case head (n) of 
                                   'A' -> if ((firstField (firstRow (gBoard game))) == bb) || ((firstField (firstRow (gBoard game))) == wb) || ((firstField (firstRow (gBoard game))) == bw) || ((firstField (firstRow (gBoard game))) == ww) then Right n else Left "Invalid input."
                                   'B' -> if ((secondField (firstRow (gBoard game))) == bb) || ((secondField (firstRow (gBoard game))) == wb) || ((secondField (firstRow (gBoard game))) == bw) || ((secondField (firstRow (gBoard game))) == ww) then Right n else Left "Invalid input."
                                   'C' -> if ((thirdField (firstRow (gBoard game))) == bb) || ((thirdField (firstRow (gBoard game))) == wb) || ((thirdField (firstRow (gBoard game))) == bw) || ((thirdField (firstRow (gBoard game))) == ww) then Right n else Left "Invalid input."
                                   'D' -> if ((fourthField (firstRow (gBoard game))) == bb) || ((fourthField (firstRow (gBoard game))) == wb) || ((fourthField (firstRow (gBoard game))) == bw) || ((fourthField (firstRow (gBoard game))) == ww) then Right n else Left "Invalid input."

evalInput3 :: String -> Either String String
evalInput3 n = do
    game <- return (mkGame) -- not in a state game yet
    case head (tail (n)) of
                            '1' -> case head (n) of 
                                   'A' -> if ((firstField (fifthRow (gBoard game))) == be) || ((firstField (fifthRow (gBoard game))) == we) then Right n else Left "Invalid input."
                                   'B' -> if ((secondField (fifthRow (gBoard game))) == be) || ((secondField (fifthRow (gBoard game))) == we) then Right n else Left "Invalid input."
                                   'C' -> if ((thirdField (fifthRow (gBoard game))) == be) || ((thirdField (fifthRow (gBoard game))) == we) then Right n else Left "Invalid input."
                                   'D' -> if ((fourthField (fifthRow (gBoard game))) == be) || ((fourthField (fifthRow (gBoard game))) == we) then Right n else Left "Invalid input."
                            '2' -> case head (n) of 
                                   'A' -> if ((firstField (fourthRow (gBoard game))) == be) || ((firstField (fourthRow (gBoard game))) == we) then Right n else Left "Invalid input."
                                   'B' -> if ((secondField (fourthRow (gBoard game))) == be) || ((secondField (fourthRow (gBoard game))) == we) then Right n else Left "Invalid input."
                                   'C' -> if ((thirdField (fourthRow (gBoard game))) == be) || ((thirdField (fourthRow (gBoard game))) == we) then Right n else Left "Invalid input."
                                   'D' -> if ((fourthField (fourthRow (gBoard game))) == be) || ((fourthField (fourthRow (gBoard game))) == we) then Right n else Left "Invalid input."
                            '3' -> case head (n) of 
                                   'A' -> if ((firstField (thirdRow (gBoard game))) == be) || ((firstField (thirdRow (gBoard game))) == we) then Right n else Left "Invalid input."
                                   'B' -> if ((secondField (thirdRow (gBoard game))) == be) || ((secondField (thirdRow (gBoard game))) == we) then Right n else Left "Invalid input."
                                   'C' -> if ((thirdField (thirdRow (gBoard game))) == be) || ((thirdField (thirdRow (gBoard game))) == we) then Right n else Left "Invalid input."
                                   'D' -> if ((fourthField (thirdRow (gBoard game))) == be) || ((fourthField (thirdRow (gBoard game))) == we) then Right n else Left "Invalid input."                                                                     
                            '4' -> case head (n) of 
                                   'A' -> if ((firstField (secondRow (gBoard game))) == be) || ((firstField (secondRow (gBoard game))) == we) then Right n else Left "Invalid input."
                                   'B' -> if ((secondField (secondRow (gBoard game))) == be) || ((secondField (secondRow (gBoard game))) == we) then Right n else Left "Invalid input."
                                   'C' -> if ((thirdField (secondRow (gBoard game))) == be) || ((thirdField (secondRow (gBoard game))) == we) then Right n else Left "Invalid input."
                                   'D' -> if ((fourthField (secondRow (gBoard game))) == be) || ((fourthField (secondRow (gBoard game))) == we) then Right n else Left "Invalid input."
                            '5' -> case head (n) of 
                                   'A' -> if ((firstField (firstRow (gBoard game))) == be) || ((firstField (firstRow (gBoard game))) == we) then Right n else Left "Invalid input."
                                   'B' -> if ((secondField (firstRow (gBoard game))) == be) || ((secondField (firstRow (gBoard game))) == we) then Right n else Left "Invalid input."
                                   'C' -> if ((thirdField (firstRow (gBoard game))) == be) || ((thirdField (firstRow (gBoard game))) == we) then Right n else Left "Invalid input."
                                   'D' -> if ((fourthField (firstRow (gBoard game))) == be) || ((fourthField (firstRow (gBoard game))) == we) then Right n else Left "Invalid input."
