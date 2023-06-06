module Main where

import Data.List ()
import Data.Char ( toUpper, digitToInt )
import Data.Either ()
import System.IO ()
--import System.Console.ANSI ( clearScreen )
import Control.Monad.State ( State, StateT, runState, runStateT, liftIO, MonadState (get), put, modify )
import Control.Monad.Trans.Except ( ExceptT, runExceptT )

import View
import Evaluation

type Error = String
type World a = StateT Game (ExceptT Error IO) a

chooseBishop :: IO String
chooseBishop = do
    putStrLn ("Which bishop do you want to move? (e.g. A1)")
    cbishop <- getLine
    return (fmap toUpper cbishop)

moveBishopTo :: IO String
moveBishopTo = do
    putStrLn ("Where do you want to move this bishop to? (e.g. B2)")
    fbishop <- getLine
    return (fmap toUpper fbishop)

{--
chosenRow :: String -> Game -> Row
chosenRow rt st = case head (tail (rt)) of
       '5' -> firstRow (gBoard st)
       '4' -> secondRow (gBoard st)
       '3' -> thirdRow (gBoard st)
       '2' -> fourthRow (gBoard st)
       '1' -> fifthRow (gBoard st)
--}

chosenField :: String -> Game -> Field
chosenField uuu vvv = case (head (tail (fmap toUpper uuu))) of
                            '5' -> case (head (fmap toUpper uuu)) of 
                                   'A' -> if ((firstField (firstRow (gBoard vvv))) == bb) then bb else if ((firstField (firstRow (gBoard vvv))) == bw) then bw else if ((firstField (firstRow (gBoard vvv))) == wb) then wb else ww
                                   'B' -> if ((secondField (firstRow (gBoard vvv))) == bb) then bb else if ((secondField (firstRow (gBoard vvv))) == bw) then bw else if ((secondField (firstRow (gBoard vvv))) == wb) then wb else ww
                                   'C' -> if ((thirdField (firstRow (gBoard vvv))) == bb) then bb else if ((thirdField (firstRow (gBoard vvv))) == bw) then bw else if ((thirdField (firstRow (gBoard vvv))) == wb) then wb else ww
                                   'D' -> if ((fourthField (firstRow (gBoard vvv))) == bb) then bb else if ((fourthField (firstRow (gBoard vvv))) == bw) then bw else if ((fourthField (firstRow (gBoard vvv))) == wb) then wb else ww
                                   _ -> bb
                            '4' -> case head (uuu) of 
                                   'A' -> if ((firstField (secondRow (gBoard vvv))) == bb) then bb else if ((firstField (secondRow (gBoard vvv))) == bw) then bw else if ((firstField (secondRow (gBoard vvv))) == wb) then wb else ww
                                   'B' -> if ((secondField (secondRow (gBoard vvv))) == bb) then bb else if ((secondField (secondRow (gBoard vvv))) == bw) then bw else if ((secondField (secondRow (gBoard vvv))) == wb) then wb else ww
                                   'C' -> if ((thirdField (secondRow (gBoard vvv))) == bb) then bb else if ((thirdField (secondRow (gBoard vvv))) == bw) then bw else if ((thirdField (secondRow (gBoard vvv))) == wb) then wb else ww
                                   'D' -> if ((fourthField (secondRow (gBoard vvv))) == bb) then bb else if ((fourthField (secondRow (gBoard vvv))) == bw) then bw else if ((fourthField (secondRow (gBoard vvv))) == wb) then wb else ww
                                   _ -> bb
                            '3' -> case head (uuu) of 
                                   'A' -> if ((firstField (thirdRow (gBoard vvv))) == bb) then bb else if ((firstField (thirdRow (gBoard vvv))) == bw) then bw else if ((firstField (thirdRow (gBoard vvv))) == wb) then wb else ww
                                   'B' -> if ((secondField (thirdRow (gBoard vvv))) == bb) then bb else if ((secondField (thirdRow (gBoard vvv))) == bw) then bw else if ((secondField (thirdRow (gBoard vvv))) == wb) then wb else ww
                                   'C' -> if ((thirdField (thirdRow (gBoard vvv))) == bb) then bb else if ((thirdField (thirdRow (gBoard vvv))) == bw) then bw else if ((thirdField (thirdRow (gBoard vvv))) == wb) then wb else ww
                                   'D' -> if ((fourthField (thirdRow (gBoard vvv))) == bb) then bb else if ((fourthField (thirdRow (gBoard vvv))) == bw) then bw else if ((fourthField (thirdRow (gBoard vvv))) == wb) then wb else ww
                                   _ -> bb
                            '2' -> case head (uuu) of 
                                   'A' -> if ((firstField (fourthRow (gBoard vvv))) == bb) then bb else if ((firstField (fourthRow (gBoard vvv))) == bw) then bw else if ((firstField (fourthRow (gBoard vvv))) == wb) then wb else ww
                                   'B' -> if ((secondField (fourthRow (gBoard vvv))) == bb) then bb else if ((secondField (fourthRow (gBoard vvv))) == bw) then bw else if ((secondField (fourthRow (gBoard vvv))) == wb) then wb else ww
                                   'C' -> if ((thirdField (fourthRow (gBoard vvv))) == bb) then bb else if ((thirdField (fourthRow (gBoard vvv))) == bw) then bw else if ((thirdField (fourthRow (gBoard vvv))) == wb) then wb else ww
                                   'D' -> if ((fourthField (fourthRow (gBoard vvv))) == bb) then bb else if ((fourthField (fourthRow (gBoard vvv))) == bw) then bw else if ((fourthField (fourthRow (gBoard vvv))) == wb) then wb else ww
                                   _ -> bb
                            '1' -> case head (uuu) of 
                                   'A' -> if ((firstField (fifthRow (gBoard vvv))) == bb) then bb else if ((firstField (fifthRow (gBoard vvv))) == bw) then bw else if ((firstField (fifthRow (gBoard vvv))) == wb) then wb else ww
                                   'B' -> if ((secondField (fifthRow (gBoard vvv))) == bb) then bb else if ((secondField (fifthRow (gBoard vvv))) == bw) then bw else if ((secondField (fifthRow (gBoard vvv))) == wb) then wb else ww
                                   'C' -> if ((thirdField (fifthRow (gBoard vvv))) == bb) then bb else if ((thirdField (fifthRow (gBoard vvv))) == bw) then bw else if ((thirdField (fifthRow (gBoard vvv))) == wb) then wb else ww
                                   'D' -> if ((fourthField (fifthRow (gBoard vvv))) == bb) then bb else if ((fourthField (fifthRow (gBoard vvv))) == bw) then bw else if ((fourthField (fifthRow (gBoard vvv))) == wb) then wb else ww
                                   _ -> bb
                            _ -> bb

{--
updateRow :: String -> Field -> Row -> Row
updateRow ab cd ef = case ((digitToInt (head (ab)))-9) of
       1 -> ((cd) : (secondField (ef)) : (thirdField (ef)) : (fourthField (ef)) : [])
       2 -> ((firstField (ef)) : (cd) : (thirdField (ef)) : (fourthField (ef)) : [])
       3 -> ((firstField (ef)) : (secondField (ef)) : (cd) : (fourthField (ef)) : [])
       4 -> ((firstField (ef)) : (secondField (ef)) : (thirdField (ef)) : (cd) : [])

updateBoard :: String -> Row -> Board -> Board
updateBoard gh ji kl = case (read (show ((digitToInt ((head (tail (gh)))))))) of
       5 -> ((ji) : (secondRow (kl)) : (thirdRow (kl)) : (fourthRow (kl)) : (fifthRow (kl)) : [])
       4 -> ((firstRow (kl)) : (ji) : (thirdRow (kl)) : (fourthRow (kl)) : (fifthRow (kl)) : [])
       3 -> ((firstRow (kl)) : (secondRow (kl)) : (ji) : (fourthRow (kl)) : (fifthRow (kl)) : [])
       2 -> ((firstRow (kl)) : (secondRow (kl)) : (thirdRow (kl)) : (ji) : (fifthRow (kl)) : [])
       1 -> ((firstRow (kl)) : (secondRow (kl)) : (thirdRow (kl)) : (fourthRow (kl)) : (ji) : [])
--}

takeBishop :: String -> Game -> Game
takeBishop uu vv = do 
        case head (tail (uu)) of 
                            '5' -> case head (uu) of 
                                   'A' -> CGame { gBoard = (((be) : (secondField (firstRow (gBoard vv))) : (thirdField (firstRow (gBoard vv))) : (fourthField (firstRow (gBoard vv))) : []) : (secondRow (gBoard vv)) : (thirdRow (gBoard vv)) : (fourthRow (gBoard vv)) : (fifthRow (gBoard vv)) : []) }
                                   'B' -> CGame { gBoard = (((firstField (firstRow (gBoard vv))) : (we) : (thirdField (firstRow (gBoard vv))) : (fourthField (firstRow (gBoard vv))) : []) : (secondRow (gBoard vv)) : (thirdRow (gBoard vv)) : (fourthRow (gBoard vv)) : (fifthRow (gBoard vv)) : []) }
                                   'C' -> CGame { gBoard = (((firstField (firstRow (gBoard vv))) : (secondField (firstRow (gBoard vv))) : (be) : (fourthField (firstRow (gBoard vv))) : []) : (secondRow (gBoard vv)) : (thirdRow (gBoard vv)) : (fourthRow (gBoard vv)) : (fifthRow (gBoard vv)) : []) }
                                   'D' -> CGame { gBoard = (((firstField (firstRow (gBoard vv))) : (secondField (firstRow (gBoard vv))) : (thirdField (firstRow (gBoard vv))) : (we) : []) : (secondRow (gBoard vv)) : (thirdRow (gBoard vv)) : (fourthRow (gBoard vv)) : (fifthRow (gBoard vv)) : []) }
                                   _ -> mkGame2
                            '4' -> case head (uu) of 
                                   'A' -> CGame { gBoard = ((firstRow (gBoard vv)) : ((we) : (secondField (secondRow (gBoard vv))) : (thirdField (secondRow (gBoard vv))) : (fourthField (secondRow (gBoard vv))) : []) : (thirdRow (gBoard vv)) : (fourthRow (gBoard vv)) : (fifthRow (gBoard vv)) : []) }
                                   'B' -> CGame { gBoard = ((firstRow (gBoard vv)) : ((firstField (secondRow (gBoard vv))) : (be) : (thirdField (secondRow (gBoard vv))) : (fourthField (secondRow (gBoard vv))) : []) : (thirdRow (gBoard vv)) : (fourthRow (gBoard vv)) : (fifthRow (gBoard vv)) : []) }
                                   'C' -> CGame { gBoard = ((firstRow (gBoard vv)) : ((firstField (secondRow (gBoard vv))) : (secondField (secondRow (gBoard vv))) : (we) : (fourthField (secondRow (gBoard vv))) : []) : (thirdRow (gBoard vv)) : (fourthRow (gBoard vv)) : (fifthRow (gBoard vv)) : []) }
                                   'D' -> CGame { gBoard = ((firstRow (gBoard vv)) : ((firstField (secondRow (gBoard vv))) : (secondField (secondRow (gBoard vv))) : (thirdField (secondRow (gBoard vv))) : (be) : []) : (thirdRow (gBoard vv)) : (fourthRow (gBoard vv)) : (fifthRow (gBoard vv)) : []) }
                                   _ -> mkGame2
                            '3' -> case head (uu) of 
                                   'A' -> CGame { gBoard = ((firstRow (gBoard vv)) : (secondRow (gBoard vv)) : ((be) : (secondField (thirdRow (gBoard vv))) : (thirdField (thirdRow (gBoard vv))) : (fourthField (thirdRow (gBoard vv))) : []) : (fourthRow (gBoard vv)) : (fifthRow (gBoard vv)) : []) }
                                   'B' -> CGame { gBoard = ((firstRow (gBoard vv)) : (secondRow (gBoard vv)) : ((firstField (thirdRow (gBoard vv))) : (we) : (thirdField (thirdRow (gBoard vv))) : (fourthField (thirdRow (gBoard vv))) : []) : (fourthRow (gBoard vv)) : (fifthRow (gBoard vv)) : []) }
                                   'C' -> CGame { gBoard = ((firstRow (gBoard vv)) : (secondRow (gBoard vv)) : ((firstField (thirdRow (gBoard vv))) : (secondField (thirdRow (gBoard vv))) : (be) : (fourthField (thirdRow (gBoard vv))) : []) : (fourthRow (gBoard vv)) : (fifthRow (gBoard vv)) : []) }
                                   'D' -> CGame { gBoard = ((firstRow (gBoard vv)) : (secondRow (gBoard vv)) : ((firstField (thirdRow (gBoard vv))) : (secondField (thirdRow (gBoard vv))) : (thirdField (thirdRow (gBoard vv))) : (we) : []) : (fourthRow (gBoard vv)) : (fifthRow (gBoard vv)) : []) }
                                   _ -> mkGame2
                            '2' -> case head (uu) of 
                                   'A' -> CGame { gBoard = ((firstRow (gBoard vv)) : (secondRow (gBoard vv)) : (thirdRow (gBoard vv)) : ((we) : (secondField (fourthRow (gBoard vv))) : (thirdField (fourthRow (gBoard vv))) : (fourthField (fourthRow (gBoard vv))) : []) : (fifthRow (gBoard vv)) : []) }
                                   'B' -> CGame { gBoard = ((firstRow (gBoard vv)) : (secondRow (gBoard vv)) : (thirdRow (gBoard vv)) : ((firstField (fourthRow (gBoard vv))) : (be) : (thirdField (fourthRow (gBoard vv))) : (fourthField (fourthRow (gBoard vv))) : []) : (fifthRow (gBoard vv)) : []) }
                                   'C' -> CGame { gBoard = ((firstRow (gBoard vv)) : (secondRow (gBoard vv)) : (thirdRow (gBoard vv)) : ((firstField (fourthRow (gBoard vv))) : (secondField (fourthRow (gBoard vv))) : (we) : (fourthField (fourthRow (gBoard vv))) : []) : (fifthRow (gBoard vv)) : []) }
                                   'D' -> CGame { gBoard = ((firstRow (gBoard vv)) : (secondRow (gBoard vv)) : (thirdRow (gBoard vv)) : ((firstField (fourthRow (gBoard vv))) : (secondField (fourthRow (gBoard vv))) : (thirdField (fourthRow (gBoard vv))) : (be) : []) : (fifthRow (gBoard vv)) : []) }
                                   _ -> mkGame2
                            '1' -> case head (uu) of 
                                   'A' -> CGame { gBoard = ((firstRow (gBoard vv)) : (secondRow (gBoard vv)) : (thirdRow (gBoard vv)) : (fourthRow (gBoard vv)) : ((be) : (secondField (fifthRow (gBoard vv))) : (thirdField (fifthRow (gBoard vv))) : (fourthField (fifthRow (gBoard vv))) : []) : []) }
                                   'B' -> CGame { gBoard = ((firstRow (gBoard vv)) : (secondRow (gBoard vv)) : (thirdRow (gBoard vv)) : (fourthRow (gBoard vv)) : ((firstField (fifthRow (gBoard vv))) : (we) : (thirdField (fifthRow (gBoard vv))) : (fourthField (fifthRow (gBoard vv))) : []) : []) }
                                   'C' -> CGame { gBoard = ((firstRow (gBoard vv)) : (secondRow (gBoard vv)) : (thirdRow (gBoard vv)) : (fourthRow (gBoard vv)) : ((firstField (fifthRow (gBoard vv))) : (secondField (fifthRow (gBoard vv))) : (be) : (fourthField (fifthRow (gBoard vv))) : []) : []) }
                                   'D' -> CGame { gBoard = ((firstRow (gBoard vv)) : (secondRow (gBoard vv)) : (thirdRow (gBoard vv)) : (fourthRow (gBoard vv)) : ((firstField (fifthRow (gBoard vv))) : (secondField (fifthRow (gBoard vv))) : (thirdField (fifthRow (gBoard vv))) : (we) : []) : []) }
                                   _ -> mkGame2
                            _ -> mkGame2

positionBishop :: String -> Field -> Game -> Game
positionBishop uuuu vvvv ssss = do 
    case head (tail (uuuu)) of 
                            '5' -> case head (uuuu) of 
                                   'A' -> CGame { gBoard = (((vvvv) : (secondField (firstRow (gBoard ssss))) : (thirdField (firstRow (gBoard ssss))) : (fourthField (firstRow (gBoard ssss))) : []) : (secondRow (gBoard ssss)) : (thirdRow (gBoard ssss)) : (fourthRow (gBoard ssss)) : (fifthRow (gBoard ssss)) : []) }
                                   'B' -> CGame { gBoard = (((firstField (firstRow (gBoard ssss))) : (vvvv) : (thirdField (firstRow (gBoard ssss))) : (fourthField (firstRow (gBoard ssss))) : []) : (secondRow (gBoard ssss)) : (thirdRow (gBoard ssss)) : (fourthRow (gBoard ssss)) : (fifthRow (gBoard ssss)) : []) }
                                   'C' -> CGame { gBoard = (((firstField (firstRow (gBoard ssss))) : (secondField (firstRow (gBoard ssss))) : (vvvv) : (fourthField (firstRow (gBoard ssss))) : []) : (secondRow (gBoard ssss)) : (thirdRow (gBoard ssss)) : (fourthRow (gBoard ssss)) : (fifthRow (gBoard ssss)) : []) }
                                   'D' -> CGame { gBoard = (((firstField (firstRow (gBoard ssss))) : (secondField (firstRow (gBoard ssss))) : (thirdField (firstRow (gBoard ssss))) : (vvvv) : []) : (secondRow (gBoard ssss)) : (thirdRow (gBoard ssss)) : (fourthRow (gBoard ssss)) : (fifthRow (gBoard ssss)) : []) }
                                   _ -> mkGame2
                            '4' -> case head (uuuu) of 
                                   'A' -> CGame { gBoard = ((firstRow (gBoard ssss)) : ((vvvv) : (secondField (secondRow (gBoard ssss))) : (thirdField (secondRow (gBoard ssss))) : (fourthField (secondRow (gBoard ssss))) : []) : (thirdRow (gBoard ssss)) : (fourthRow (gBoard ssss)) : (fifthRow (gBoard ssss)) : []) }
                                   'B' -> CGame { gBoard = ((firstRow (gBoard ssss)) : ((firstField (secondRow (gBoard ssss))) : (vvvv) : (thirdField (secondRow (gBoard ssss))) : (fourthField (secondRow (gBoard ssss))) : []) : (thirdRow (gBoard ssss)) : (fourthRow (gBoard ssss)) : (fifthRow (gBoard ssss)) : []) }
                                   'C' -> CGame { gBoard = ((firstRow (gBoard ssss)) : ((firstField (secondRow (gBoard ssss))) : (secondField (secondRow (gBoard ssss))) : (vvvv) : (fourthField (secondRow (gBoard ssss))) : []) : (thirdRow (gBoard ssss)) : (fourthRow (gBoard ssss)) : (fifthRow (gBoard ssss)) : []) }
                                   'D' -> CGame { gBoard = ((firstRow (gBoard ssss)) : ((firstField (secondRow (gBoard ssss))) : (secondField (secondRow (gBoard ssss))) : (thirdField (secondRow (gBoard ssss))) : (vvvv) : []) : (thirdRow (gBoard ssss)) : (fourthRow (gBoard ssss)) : (fifthRow (gBoard ssss)) : []) }
                                   _ -> mkGame2
                            '3' -> case head (uuuu) of 
                                   'A' -> CGame { gBoard = ((firstRow (gBoard ssss)) : (secondRow (gBoard ssss)) : ((vvvv) : (secondField (thirdRow (gBoard ssss))) : (thirdField (thirdRow (gBoard ssss))) : (fourthField (thirdRow (gBoard ssss))) : []) : (fourthRow (gBoard ssss)) : (fifthRow (gBoard ssss)) : []) }
                                   'B' -> CGame { gBoard = ((firstRow (gBoard ssss)) : (secondRow (gBoard ssss)) : ((firstField (thirdRow (gBoard ssss))) : (vvvv) : (thirdField (thirdRow (gBoard ssss))) : (fourthField (thirdRow (gBoard ssss))) : []) : (fourthRow (gBoard ssss)) : (fifthRow (gBoard ssss)) : []) }
                                   'C' -> CGame { gBoard = ((firstRow (gBoard ssss)) : (secondRow (gBoard ssss)) : ((firstField (thirdRow (gBoard ssss))) : (secondField (thirdRow (gBoard ssss))) : (vvvv) : (fourthField (thirdRow (gBoard ssss))) : []) : (fourthRow (gBoard ssss)) : (fifthRow (gBoard ssss)) : []) }
                                   'D' -> CGame { gBoard = ((firstRow (gBoard ssss)) : (secondRow (gBoard ssss)) : ((firstField (thirdRow (gBoard ssss))) : (secondField (thirdRow (gBoard ssss))) : (thirdField (thirdRow (gBoard ssss))) : (vvvv) : []) : (fourthRow (gBoard ssss)) : (fifthRow (gBoard ssss)) : []) }
                                   _ -> mkGame2
                            '2' -> case head (uuuu) of 
                                   'A' -> CGame { gBoard = ((firstRow (gBoard ssss)) : (secondRow (gBoard ssss)) : (thirdRow (gBoard ssss)) : ((vvvv) : (secondField (fourthRow (gBoard ssss))) : (thirdField (fourthRow (gBoard ssss))) : (fourthField (fourthRow (gBoard ssss))) : []) : (fifthRow (gBoard ssss)) : []) }
                                   'B' -> CGame { gBoard = ((firstRow (gBoard ssss)) : (secondRow (gBoard ssss)) : (thirdRow (gBoard ssss)) : ((firstField (fourthRow (gBoard ssss))) : (vvvv) : (thirdField (fourthRow (gBoard ssss))) : (fourthField (fourthRow (gBoard ssss))) : []) : (fifthRow (gBoard ssss)) : []) }
                                   'C' -> CGame { gBoard = ((firstRow (gBoard ssss)) : (secondRow (gBoard ssss)) : (thirdRow (gBoard ssss)) : ((firstField (fourthRow (gBoard ssss))) : (secondField (fourthRow (gBoard ssss))) : (vvvv) : (fourthField (fourthRow (gBoard ssss))) : []) : (fifthRow (gBoard ssss)) : []) }
                                   'D' -> CGame { gBoard = ((firstRow (gBoard ssss)) : (secondRow (gBoard ssss)) : (thirdRow (gBoard ssss)) : ((firstField (fourthRow (gBoard ssss))) : (secondField (fourthRow (gBoard ssss))) : (thirdField (fourthRow (gBoard ssss))) : (vvvv) : []) : (fifthRow (gBoard ssss)) : []) }
                                   _ -> mkGame2
                            '1' -> case head (uuuu) of 
                                   'A' -> CGame { gBoard = ((firstRow (gBoard ssss)) : (secondRow (gBoard ssss)) : (thirdRow (gBoard ssss)) : (fourthRow (gBoard ssss)) : ((vvvv) : (secondField (fifthRow (gBoard ssss))) : (thirdField (fifthRow (gBoard ssss))) : (fourthField (fifthRow (gBoard ssss))) : []) : []) }
                                   'B' -> CGame { gBoard = ((firstRow (gBoard ssss)) : (secondRow (gBoard ssss)) : (thirdRow (gBoard ssss)) : (fourthRow (gBoard ssss)) : ((firstField (fifthRow (gBoard ssss))) : (vvvv) : (thirdField (fifthRow (gBoard ssss))) : (fourthField (fifthRow (gBoard ssss))) : []) : []) }
                                   'C' -> CGame { gBoard = ((firstRow (gBoard ssss)) : (secondRow (gBoard ssss)) : (thirdRow (gBoard ssss)) : (fourthRow (gBoard ssss)) : ((firstField (fifthRow (gBoard ssss))) : (secondField (fifthRow (gBoard ssss))) : (vvvv) : (fourthField (fifthRow (gBoard ssss))) : []) : []) }
                                   'D' -> CGame { gBoard = ((firstRow (gBoard ssss)) : (secondRow (gBoard ssss)) : (thirdRow (gBoard ssss)) : (fourthRow (gBoard ssss)) : ((firstField (fifthRow (gBoard ssss))) : (secondField (fifthRow (gBoard ssss))) : (thirdField (fifthRow (gBoard ssss))) : (vvvv) : []) : []) }
                                   _ -> mkGame2
                            _ -> mkGame2

{--
playMove :: World ()
playMove = do
    game <- return (mkGame)
    liftIO $ do
        choice1 <- chooseBishop
        choice2 <- moveBishopTo
        print (evalInput choice1)
        print (evalInput2 choice1)   
        print (evalInput choice2)
        print (evalInput3 choice2)   
--}

playMove :: StateT Game IO ()
playMove = do
    --newBoard <- return (gBoard (snd (runState (state o p) q)))
    --liftIO $ putStrLn (printBoard (newBoard))
    qqq1 <- get
    choice1 <- liftIO $ chooseBishop
    sss <- return (chosenField choice1 qqq1)
    modify (\x -> (takeBishop choice1 qqq1))
    qqq2 <- get
    choice2 <- liftIO $ moveBishopTo
    modify (\x -> (positionBishop choice2 sss qqq2))
    qqq3 <- get
    liftIO $ putStrLn (printBoard (gBoard qqq3))
    playMove
    
    --playMove choice21 choice22 (snd (runState (state choice21 choice22) qqq))
    --liftIO $ print (evalInput choice21)
    --print (evalInput2 choice21)   
    --print (evalInput choice22)
    --print (evalInput3 choice22)   
    --print (decideColorBishop choice1 game)

main :: IO ()
main = do
    setup
    game <- return (mkGame)
    --choice11 <- chooseBishop
    --choice12 <- moveBishopTo
--    return $! (snd (runState (state choice11 choice12) $! game))
--    playMove choice11 choice12 game
    runStateT playMove game
    --putStrLn (printBoard (updateBoard choice11 (updateRow choice11 (chosenField choice12 game) (chosenRow choice12 game)) (gBoard game)))
    --state <- return (firstField (firstRow (gBoard game)))
    --print (state)
    --runStateT (runExceptT playMove "error") game
    return ()

