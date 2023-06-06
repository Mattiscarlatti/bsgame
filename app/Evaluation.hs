module Evaluation where

import View

evalInput1 :: String -> Either String String
evalInput1 j = do 
    if (length j == 2) then if 
        (elem (head (j)) "ABCD") then if
            (elem (head (tail (j))) "12345") then Right j else Left "Invalid input."
        else Left "Invalid input."
    else Left "Invalid input."
 
evalInput2 :: String -> Game -> Either String String
evalInput2 n m = do
    case head (tail (n)) of
                            '1' -> case head (n) of 
                                   'A' -> if ((firstField (fifthRow (gBoard m))) == bb) || ((firstField (fifthRow (gBoard m))) == wb) || ((firstField (fifthRow (gBoard m))) == bw) || ((firstField (fifthRow (gBoard m))) == ww) then Right n else Left "Invalid input."
                                   'B' -> if ((secondField (fifthRow (gBoard m))) == bb) || ((secondField (fifthRow (gBoard m))) == wb) || ((secondField (fifthRow (gBoard m))) == bw) || ((secondField (fifthRow (gBoard m))) == ww) then Right n else Left "Invalid input."
                                   'C' -> if ((thirdField (fifthRow (gBoard m))) == bb) || ((thirdField (fifthRow (gBoard m))) == wb) || ((thirdField (fifthRow (gBoard m))) == bw) || ((thirdField (fifthRow (gBoard m))) == ww) then Right n else Left "Invalid input."
                                   'D' -> if ((fourthField (fifthRow (gBoard m))) == bb) || ((fourthField (fifthRow (gBoard m))) == wb) || ((fourthField (fifthRow (gBoard m))) == bw) || ((fourthField (fifthRow (gBoard m))) == ww) then Right n else Left "Invalid input."
                            '2' -> case head (n) of 
                                   'A' -> if ((firstField (fourthRow (gBoard m))) == bb) || ((firstField (fourthRow (gBoard m))) == wb) || ((firstField (fourthRow (gBoard m))) == bw) || ((firstField (fourthRow (gBoard m))) == ww) then Right n else Left "Invalid input."
                                   'B' -> if ((secondField (fourthRow (gBoard m))) == bb) || ((secondField (fourthRow (gBoard m))) == wb) || ((secondField (fourthRow (gBoard m))) == bw) || ((secondField (fourthRow (gBoard m))) == ww) then Right n else Left "Invalid input."
                                   'C' -> if ((thirdField (fourthRow (gBoard m))) == bb) || ((thirdField (fourthRow (gBoard m))) == wb) || ((thirdField (fourthRow (gBoard m))) == bw) || ((thirdField (fourthRow (gBoard m))) == ww) then Right n else Left "Invalid input."
                                   'D' -> if ((fourthField (fourthRow (gBoard m))) == bb) || ((fourthField (fourthRow (gBoard m))) == wb) || ((fourthField (fourthRow (gBoard m))) == bw) || ((fourthField (fourthRow (gBoard m))) == ww) then Right n else Left "Invalid input."
                            '3' -> case head (n) of 
                                   'A' -> if ((firstField (thirdRow (gBoard m))) == bb) || ((firstField (thirdRow (gBoard m))) == wb) || ((firstField (thirdRow (gBoard m))) == bw) || ((firstField (thirdRow (gBoard m))) == ww) then Right n else Left "Invalid input."
                                   'B' -> if ((secondField (thirdRow (gBoard m))) == bb) || ((secondField (thirdRow (gBoard m))) == wb) || ((secondField (thirdRow (gBoard m))) == bw) || ((secondField (thirdRow (gBoard m))) == ww) then Right n else Left "Invalid input."
                                   'C' -> if ((thirdField (thirdRow (gBoard m))) == bb) || ((thirdField (thirdRow (gBoard m))) == wb) || ((thirdField (thirdRow (gBoard m))) == bw) || ((thirdField (thirdRow (gBoard m))) == ww) then Right n else Left "Invalid input."
                                   'D' -> if ((fourthField (thirdRow (gBoard m))) == bb) || ((fourthField (thirdRow (gBoard m))) == wb) || ((fourthField (thirdRow (gBoard m))) == bw) || ((fourthField (thirdRow (gBoard m))) == ww) then Right n else Left "Invalid input."                                                                     
                            '4' -> case head (n) of 
                                   'A' -> if ((firstField (secondRow (gBoard m))) == bb) || ((firstField (secondRow (gBoard m))) == wb) || ((firstField (secondRow (gBoard m))) == bw) || ((firstField (secondRow (gBoard m))) == ww) then Right n else Left "Invalid input."
                                   'B' -> if ((secondField (secondRow (gBoard m))) == bb) || ((secondField (secondRow (gBoard m))) == wb) || ((secondField (secondRow (gBoard m))) == bw) || ((secondField (secondRow (gBoard m))) == ww) then Right n else Left "Invalid input."
                                   'C' -> if ((thirdField (secondRow (gBoard m))) == bb) || ((thirdField (secondRow (gBoard m))) == wb) || ((thirdField (secondRow (gBoard m))) == bw) || ((thirdField (secondRow (gBoard m))) == ww) then Right n else Left "Invalid input."
                                   'D' -> if ((fourthField (secondRow (gBoard m))) == bb) || ((fourthField (secondRow (gBoard m))) == wb) || ((fourthField (secondRow (gBoard m))) == bw) || ((fourthField (secondRow (gBoard m))) == ww) then Right n else Left "Invalid input."
                            '5' -> case head (n) of 
                                   'A' -> if ((firstField (firstRow (gBoard m))) == bb) || ((firstField (firstRow (gBoard m))) == wb) || ((firstField (firstRow (gBoard m))) == bw) || ((firstField (firstRow (gBoard m))) == ww) then Right n else Left "Invalid input."
                                   'B' -> if ((secondField (firstRow (gBoard m))) == bb) || ((secondField (firstRow (gBoard m))) == wb) || ((secondField (firstRow (gBoard m))) == bw) || ((secondField (firstRow (gBoard m))) == ww) then Right n else Left "Invalid input."
                                   'C' -> if ((thirdField (firstRow (gBoard m))) == bb) || ((thirdField (firstRow (gBoard m))) == wb) || ((thirdField (firstRow (gBoard m))) == bw) || ((thirdField (firstRow (gBoard m))) == ww) then Right n else Left "Invalid input."
                                   'D' -> if ((fourthField (firstRow (gBoard m))) == bb) || ((fourthField (firstRow (gBoard m))) == wb) || ((fourthField (firstRow (gBoard m))) == bw) || ((fourthField (firstRow (gBoard m))) == ww) then Right n else Left "Invalid input."

evalInput3 :: String -> Game -> Either String String
evalInput3 n m = do
    case head (tail (n)) of
                            '1' -> case head (n) of 
                                   'A' -> if ((firstField (fifthRow (gBoard m))) == be) || ((firstField (fifthRow (gBoard m))) == we) then Right n else Left "Invalid input."
                                   'B' -> if ((secondField (fifthRow (gBoard m))) == be) || ((secondField (fifthRow (gBoard m))) == we) then Right n else Left "Invalid input."
                                   'C' -> if ((thirdField (fifthRow (gBoard m))) == be) || ((thirdField (fifthRow (gBoard m))) == we) then Right n else Left "Invalid input."
                                   'D' -> if ((fourthField (fifthRow (gBoard m))) == be) || ((fourthField (fifthRow (gBoard m))) == we) then Right n else Left "Invalid input."
                            '2' -> case head (n) of 
                                   'A' -> if ((firstField (fourthRow (gBoard m))) == be) || ((firstField (fourthRow (gBoard m))) == we) then Right n else Left "Invalid input."
                                   'B' -> if ((secondField (fourthRow (gBoard m))) == be) || ((secondField (fourthRow (gBoard m))) == we) then Right n else Left "Invalid input."
                                   'C' -> if ((thirdField (fourthRow (gBoard m))) == be) || ((thirdField (fourthRow (gBoard m))) == we) then Right n else Left "Invalid input."
                                   'D' -> if ((fourthField (fourthRow (gBoard m))) == be) || ((fourthField (fourthRow (gBoard m))) == we) then Right n else Left "Invalid input."
                            '3' -> case head (n) of 
                                   'A' -> if ((firstField (thirdRow (gBoard m))) == be) || ((firstField (thirdRow (gBoard m))) == we) then Right n else Left "Invalid input."
                                   'B' -> if ((secondField (thirdRow (gBoard m))) == be) || ((secondField (thirdRow (gBoard m))) == we) then Right n else Left "Invalid input."
                                   'C' -> if ((thirdField (thirdRow (gBoard m))) == be) || ((thirdField (thirdRow (gBoard m))) == we) then Right n else Left "Invalid input."
                                   'D' -> if ((fourthField (thirdRow (gBoard m))) == be) || ((fourthField (thirdRow (gBoard m))) == we) then Right n else Left "Invalid input."                                                                     
                            '4' -> case head (n) of 
                                   'A' -> if ((firstField (secondRow (gBoard m))) == be) || ((firstField (secondRow (gBoard m))) == we) then Right n else Left "Invalid input."
                                   'B' -> if ((secondField (secondRow (gBoard m))) == be) || ((secondField (secondRow (gBoard m))) == we) then Right n else Left "Invalid input."
                                   'C' -> if ((thirdField (secondRow (gBoard m))) == be) || ((thirdField (secondRow (gBoard m))) == we) then Right n else Left "Invalid input."
                                   'D' -> if ((fourthField (secondRow (gBoard m))) == be) || ((fourthField (secondRow (gBoard m))) == we) then Right n else Left "Invalid input."
                            '5' -> case head (n) of 
                                   'A' -> if ((firstField (firstRow (gBoard m))) == be) || ((firstField (firstRow (gBoard m))) == we) then Right n else Left "Invalid input."
                                   'B' -> if ((secondField (firstRow (gBoard m))) == be) || ((secondField (firstRow (gBoard m))) == we) then Right n else Left "Invalid input."
                                   'C' -> if ((thirdField (firstRow (gBoard m))) == be) || ((thirdField (firstRow (gBoard m))) == we) then Right n else Left "Invalid input."
                                   'D' -> if ((fourthField (firstRow (gBoard m))) == be) || ((fourthField (firstRow (gBoard m))) == we) then Right n else Left "Invalid input."

evalInput4 :: String -> Field -> Game -> Either String String
evalInput4 n o m = do 
           case ((o == bw) || (o == ww)) of
              True -> do
                     case head (tail (n)) of
                            '1' -> case head (n) of 
                                   'A' -> if (((secondField (fourthRow (gBoard m))) == bb) || ((thirdField (thirdRow (gBoard m))) == bb) || ((fourthField (secondRow (gBoard m))) == bb)) then Left "Invalid input." else Right n
                                   'B' -> if (((firstField (fourthRow (gBoard m))) == wb) || ((thirdField (fourthRow (gBoard m))) == wb) || ((fourthField (thirdRow (gBoard m))) == wb)) then Left "Invalid input." else Right n
                                   'C' -> if (((firstField (thirdRow (gBoard m))) == bb) || ((secondField (fourthRow (gBoard m))) == bb) || ((fourthField (fourthRow (gBoard m))) == bb)) then Left "Invalid input." else Right n
                                   'D' -> if (((firstField (secondRow (gBoard m))) == wb) || ((secondField (thirdRow (gBoard m))) == wb) || ((thirdField (fourthRow (gBoard m))) == wb)) then Left "Invalid input." else Right n
                            '2' -> case head (n) of 
                                   'A' -> if (((secondField (fifthRow (gBoard m))) == wb) || ((secondField (thirdRow (gBoard m))) == wb) || ((thirdField (secondRow (gBoard m))) == wb) || ((fourthField (firstRow (gBoard m))) == wb)) then Left "Invalid input." else Right n
                                   'B' -> if (((firstField (fifthRow (gBoard m))) == bb) || ((firstField (thirdRow (gBoard m))) == bb) || ((thirdField (fifthRow (gBoard m))) == bb) || ((thirdField (thirdRow (gBoard m))) == bb) || ((fourthField (secondRow (gBoard m))) == bb)) then Left "Invalid input." else Right n
                                   'C' -> if (((firstField (secondRow (gBoard m))) == wb) || ((secondField (thirdRow (gBoard m))) == wb) || ((secondField (fifthRow (gBoard m))) == wb) || ((fourthField (thirdRow (gBoard m))) == wb) || ((fourthField (fifthRow (gBoard m))) == bb)) then Left "Invalid input." else Right n
                                   'D' -> if (((firstField (firstRow (gBoard m))) == bb) || ((secondField (secondRow (gBoard m))) == bb) || ((thirdField (thirdRow (gBoard m))) == bb) || ((thirdField (fifthRow (gBoard m))) == bb)) then Left "Invalid input." else Right n
                            '3' -> case head (n) of 
                                   'A' -> if (((secondField (secondRow (gBoard m))) == bb) || ((secondField (fourthRow (gBoard m))) == bb) || ((thirdField (firstRow (gBoard m))) == bb) || ((thirdField (fifthRow (gBoard m))) == bb)) then Left "Invalid input." else Right n
                                   'B' -> if (((firstField (secondRow (gBoard m))) == wb) || ((firstField (fourthRow (gBoard m))) == wb) || ((thirdField (secondRow (gBoard m))) == wb) || ((thirdField (fourthRow (gBoard m))) == wb) || ((fourthField (firstRow (gBoard m))) == wb) || ((fourthField (fifthRow (gBoard m))) == wb)) then Left "Invalid input." else Right n
                                   'C' -> if (((firstField (firstRow (gBoard m))) == bb) || ((firstField (fifthRow (gBoard m))) == bb) || ((secondField (secondRow (gBoard m))) == bb) || ((secondField (fourthRow (gBoard m))) == bb) || ((fourthField (secondRow (gBoard m))) == bb) || ((fourthField (fourthRow (gBoard m))) == bb)) then Left "Invalid input." else Right n
                                   'D' -> if (((secondField (firstRow (gBoard m))) == wb) || ((secondField (fifthRow (gBoard m))) == wb) || ((thirdField (secondRow (gBoard m))) == wb) || ((thirdField (fourthRow (gBoard m))) == wb)) then Left "Invalid input." else Right n
                            '4' -> case head (n) of 
                                   'A' -> if (((secondField (firstRow (gBoard m))) == wb) || ((secondField (thirdRow (gBoard m))) == wb) || ((thirdField (fourthRow (gBoard m))) == wb) || ((fourthField (fifthRow (gBoard m))) == wb)) then Left "Invalid input." else Right n
                                   'B' -> if (((firstField (firstRow (gBoard m))) == bb) || ((firstField (thirdRow (gBoard m))) == bb) || ((thirdField (firstRow (gBoard m))) == bb) || ((thirdField (thirdRow (gBoard m))) == bb) || ((fourthField (fourthRow (gBoard m))) == bb)) then Left "Invalid input." else Right n
                                   'C' -> if (((firstField (fourthRow (gBoard m))) == wb) || ((secondField (thirdRow (gBoard m))) == wb) || ((secondField (firstRow (gBoard m))) == wb) || ((fourthField (firstRow (gBoard m))) == wb) || ((fourthField (thirdRow (gBoard m))) == wb)) then Left "Invalid input." else Right n
                                   'D' -> if (((firstField (fifthRow (gBoard m))) == bb) || ((secondField (fourthRow (gBoard m))) == bb) || ((thirdField (thirdRow (gBoard m))) == bb) || ((thirdField (firstRow (gBoard m))) == bb)) then Left "Invalid input." else Right n
                            '5' -> case head (n) of 
                                   'A' -> if (((secondField (secondRow (gBoard m))) == bb) || ((thirdField (thirdRow (gBoard m))) == bb) || ((fourthField (fourthRow (gBoard m))) == bb)) then Left "Invalid input." else Right n
                                   'B' -> if (((firstField (secondRow (gBoard m))) == wb) || ((thirdField (secondRow (gBoard m))) == wb) || ((fourthField (thirdRow (gBoard m))) == wb)) then Left "Invalid input." else Right n
                                   'C' -> if (((firstField (thirdRow (gBoard m))) == bb) || ((secondField (secondRow (gBoard m))) == bb) || ((fourthField (secondRow (gBoard m))) == bb)) then Left "Invalid input." else Right n
                                   'D' -> if (((firstField (fourthRow (gBoard m))) == wb) || ((secondField (thirdRow (gBoard m))) == wb) || ((thirdField (secondRow (gBoard m))) == wb)) then Left "Invalid input." else Right n
              False -> do 
                     case head (tail (n)) of
                            '1' -> case head (n) of 
                                   'A' -> if (((secondField (fourthRow (gBoard m))) == bw) || ((thirdField (thirdRow (gBoard m))) == bw) || ((fourthField (secondRow (gBoard m))) == bw)) then Left "Invalid input." else Right n
                                   'B' -> if (((firstField (fourthRow (gBoard m))) == ww) || ((thirdField (fourthRow (gBoard m))) == ww) || ((fourthField (thirdRow (gBoard m))) == ww)) then Left "Invalid input." else Right n
                                   'C' -> if (((firstField (thirdRow (gBoard m))) == bw) || ((secondField (fourthRow (gBoard m))) == bw) || ((fourthField (fourthRow (gBoard m))) == bw)) then Left "Invalid input." else Right n
                                   'D' -> if (((firstField (secondRow (gBoard m))) == ww) || ((secondField (thirdRow (gBoard m))) == ww) || ((thirdField (fourthRow (gBoard m))) == ww)) then Left "Invalid input." else Right n
                            '2' -> case head (n) of 
                                   'A' -> if (((secondField (fifthRow (gBoard m))) == ww) || ((secondField (thirdRow (gBoard m))) == ww) || ((thirdField (secondRow (gBoard m))) == ww) || ((fourthField (firstRow (gBoard m))) == ww)) then Left "Invalid input." else Right n
                                   'B' -> if (((firstField (fifthRow (gBoard m))) == bw) || ((firstField (thirdRow (gBoard m))) == bw) || ((thirdField (fifthRow (gBoard m))) == bw) || ((thirdField (thirdRow (gBoard m))) == bw) || ((fourthField (secondRow (gBoard m))) == bw)) then Left "Invalid input." else Right n
                                   'C' -> if (((firstField (secondRow (gBoard m))) == ww) || ((secondField (thirdRow (gBoard m))) == ww) || ((secondField (fifthRow (gBoard m))) == ww) || ((fourthField (thirdRow (gBoard m))) == ww) || ((fourthField (fifthRow (gBoard m))) == bw)) then Left "Invalid input." else Right n
                                   'D' -> if (((firstField (firstRow (gBoard m))) == bw) || ((secondField (secondRow (gBoard m))) == bw) || ((thirdField (thirdRow (gBoard m))) == bw) || ((thirdField (fifthRow (gBoard m))) == bw)) then Left "Invalid input." else Right n
                            '3' -> case head (n) of 
                                   'A' -> if (((secondField (secondRow (gBoard m))) == bw) || ((secondField (fourthRow (gBoard m))) == bw) || ((thirdField (firstRow (gBoard m))) == bw) || ((thirdField (fifthRow (gBoard m))) == bw)) then Left "Invalid input." else Right n
                                   'B' -> if (((firstField (secondRow (gBoard m))) == ww) || ((firstField (fourthRow (gBoard m))) == ww) || ((thirdField (secondRow (gBoard m))) == ww) || ((thirdField (fourthRow (gBoard m))) == ww) || ((fourthField (firstRow (gBoard m))) == ww) || ((fourthField (fifthRow (gBoard m))) == ww)) then Left "Invalid input." else Right n
                                   'C' -> if (((firstField (firstRow (gBoard m))) == bw) || ((firstField (fifthRow (gBoard m))) == bw) || ((secondField (secondRow (gBoard m))) == bw) || ((secondField (fourthRow (gBoard m))) == bw) || ((fourthField (secondRow (gBoard m))) == bw) || ((fourthField (fourthRow (gBoard m))) == bw)) then Left "Invalid input." else Right n
                                   'D' -> if (((secondField (firstRow (gBoard m))) == ww) || ((secondField (fifthRow (gBoard m))) == ww) || ((thirdField (secondRow (gBoard m))) == ww) || ((thirdField (fourthRow (gBoard m))) == ww)) then Left "Invalid input." else Right n
                            '4' -> case head (n) of 
                                   'A' -> if (((secondField (firstRow (gBoard m))) == ww) || ((secondField (thirdRow (gBoard m))) == ww) || ((thirdField (fourthRow (gBoard m))) == ww) || ((fourthField (fifthRow (gBoard m))) == ww)) then Left "Invalid input." else Right n
                                   'B' -> if (((firstField (firstRow (gBoard m))) == bw) || ((firstField (thirdRow (gBoard m))) == bw) || ((thirdField (firstRow (gBoard m))) == bw) || ((thirdField (thirdRow (gBoard m))) == bw) || ((fourthField (fourthRow (gBoard m))) == bw)) then Left "Invalid input." else Right n
                                   'C' -> if (((firstField (fourthRow (gBoard m))) == ww) || ((secondField (thirdRow (gBoard m))) == ww) || ((secondField (firstRow (gBoard m))) == ww) || ((fourthField (firstRow (gBoard m))) == ww) || ((fourthField (thirdRow (gBoard m))) == ww)) then Left "Invalid input." else Right n
                                   'D' -> if (((firstField (fifthRow (gBoard m))) == bw) || ((secondField (fourthRow (gBoard m))) == bw) || ((thirdField (thirdRow (gBoard m))) == bw) || ((thirdField (firstRow (gBoard m))) == bw)) then Left "Invalid input." else Right n
                            '5' -> case head (n) of 
                                   'A' -> if (((secondField (secondRow (gBoard m))) == bw) || ((thirdField (thirdRow (gBoard m))) == bw) || ((fourthField (fourthRow (gBoard m))) == bw)) then Left "Invalid input." else Right n
                                   'B' -> if (((firstField (secondRow (gBoard m))) == ww) || ((thirdField (secondRow (gBoard m))) == ww) || ((fourthField (thirdRow (gBoard m))) == ww)) then Left "Invalid input." else Right n
                                   'C' -> if (((firstField (thirdRow (gBoard m))) == bw) || ((secondField (secondRow (gBoard m))) == bw) || ((fourthField (secondRow (gBoard m))) == bw)) then Left "Invalid input." else Right n
                                   'D' -> if (((firstField (fourthRow (gBoard m))) == ww) || ((secondField (thirdRow (gBoard m))) == ww) || ((thirdField (secondRow (gBoard m))) == ww)) then Left "Invalid input." else Right n

evalInput5 :: Field -> Field -> Either String String
evalInput5 mn nm = do 
       case mn of 
              bb -> if (nm == be) then Right "OK" else Left "Invalid input."
              bw -> if (nm == be) then Right "OK" else Left "Invalid input."
              wb -> if (nm == we) then Right "OK" else Left "Invalid input."
              ww -> if (nm == we) then Right "OK" else Left "Invalid input."
              _ -> Left "Invalid input."

