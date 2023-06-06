module Main where

import Data.List ()
import Data.Char ( toUpper )
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
chosenField uuu vvv = case head (tail (uuu)) of
                            '5' -> case head (uuu) of 
                                   'A' -> if ((firstField (firstRow (gBoard vvv))) == bb) then bb else if ((firstField (firstRow (gBoard vvv))) == bw) then bw else if ((firstField (firstRow (gBoard vvv))) == wb) then wb else if ((firstField (firstRow (gBoard vvv))) == ww) then ww else if ((firstField (firstRow (gBoard vvv))) == we) then we else be
                                   'B' -> if ((secondField (firstRow (gBoard vvv))) == bb) then bb else if ((secondField (firstRow (gBoard vvv))) == bw) then bw else if ((secondField (firstRow (gBoard vvv))) == wb) then wb else if ((secondField (firstRow (gBoard vvv))) == ww) then ww else if ((secondField (firstRow (gBoard vvv))) == we) then we else be
                                   'C' -> if ((thirdField (firstRow (gBoard vvv))) == bb) then bb else if ((thirdField (firstRow (gBoard vvv))) == bw) then bw else if ((thirdField (firstRow (gBoard vvv))) == wb) then wb else if ((thirdField (firstRow (gBoard vvv))) == ww) then ww else if ((thirdField (firstRow (gBoard vvv))) == we) then we else be
                                   'D' -> if ((fourthField (firstRow (gBoard vvv))) == bb) then bb else if ((fourthField (firstRow (gBoard vvv))) == bw) then bw else if ((fourthField (firstRow (gBoard vvv))) == wb) then wb else if ((fourthField (firstRow (gBoard vvv))) == ww) then ww else if ((fourthField (firstRow (gBoard vvv))) == we) then we else be
                                   _ -> bb
                            '4' -> case head (uuu) of 
                                   'A' -> if ((firstField (secondRow (gBoard vvv))) == bb) then bb else if ((firstField (secondRow (gBoard vvv))) == bw) then bw else if ((firstField (secondRow (gBoard vvv))) == wb) then wb else if ((firstField (secondRow (gBoard vvv))) == ww) then ww else if ((firstField (secondRow (gBoard vvv))) == we) then we else be
                                   'B' -> if ((secondField (secondRow (gBoard vvv))) == bb) then bb else if ((secondField (secondRow (gBoard vvv))) == bw) then bw else if ((secondField (secondRow (gBoard vvv))) == wb) then wb else if ((secondField (secondRow (gBoard vvv))) == ww) then ww else if ((secondField (secondRow (gBoard vvv))) == we) then we else be
                                   'C' -> if ((thirdField (secondRow (gBoard vvv))) == bb) then bb else if ((thirdField (secondRow (gBoard vvv))) == bw) then bw else if ((thirdField (secondRow (gBoard vvv))) == wb) then wb else if ((thirdField (secondRow (gBoard vvv))) == ww) then ww else if ((thirdField (secondRow (gBoard vvv))) == we) then we else be
                                   'D' -> if ((fourthField (secondRow (gBoard vvv))) == bb) then bb else if ((fourthField (secondRow (gBoard vvv))) == bw) then bw else if ((fourthField (secondRow (gBoard vvv))) == wb) then wb else if ((fourthField (secondRow (gBoard vvv))) == ww) then ww else if ((fourthField (secondRow (gBoard vvv))) == we) then we else be
                                   _ -> bb
                            '3' -> case head (uuu) of 
                                   'A' -> if ((firstField (thirdRow (gBoard vvv))) == bb) then bb else if ((firstField (thirdRow (gBoard vvv))) == bw) then bw else if ((firstField (thirdRow (gBoard vvv))) == wb) then wb else if ((firstField (thirdRow (gBoard vvv))) == ww) then ww else if ((firstField (thirdRow (gBoard vvv))) == we) then we else be
                                   'B' -> if ((secondField (thirdRow (gBoard vvv))) == bb) then bb else if ((secondField (thirdRow (gBoard vvv))) == bw) then bw else if ((secondField (thirdRow (gBoard vvv))) == wb) then wb else if ((secondField (thirdRow (gBoard vvv))) == ww) then ww else if ((secondField (thirdRow (gBoard vvv))) == we) then we else be
                                   'C' -> if ((thirdField (thirdRow (gBoard vvv))) == bb) then bb else if ((thirdField (thirdRow (gBoard vvv))) == bw) then bw else if ((thirdField (thirdRow (gBoard vvv))) == wb) then wb else if ((thirdField (thirdRow (gBoard vvv))) == ww) then ww else if ((thirdField (thirdRow (gBoard vvv))) == we) then we else be
                                   'D' -> if ((fourthField (thirdRow (gBoard vvv))) == bb) then bb else if ((fourthField (thirdRow (gBoard vvv))) == bw) then bw else if ((fourthField (thirdRow (gBoard vvv))) == wb) then wb else if ((fourthField (thirdRow (gBoard vvv))) == ww) then ww else if ((fourthField (thirdRow (gBoard vvv))) == we) then we else be
                                   _ -> bb
                            '2' -> case head (uuu) of 
                                   'A' -> if ((firstField (fourthRow (gBoard vvv))) == bb) then bb else if ((firstField (fourthRow (gBoard vvv))) == bw) then bw else if ((firstField (fourthRow (gBoard vvv))) == wb) then wb else if ((firstField (fourthRow (gBoard vvv))) == ww) then ww else if ((firstField (fourthRow (gBoard vvv))) == we) then we else be
                                   'B' -> if ((secondField (fourthRow (gBoard vvv))) == bb) then bb else if ((secondField (fourthRow (gBoard vvv))) == bw) then bw else if ((secondField (fourthRow (gBoard vvv))) == wb) then wb else if ((secondField (fourthRow (gBoard vvv))) == ww) then ww else if ((secondField (fourthRow (gBoard vvv))) == we) then we else be
                                   'C' -> if ((thirdField (fourthRow (gBoard vvv))) == bb) then bb else if ((thirdField (fourthRow (gBoard vvv))) == bw) then bw else if ((thirdField (fourthRow (gBoard vvv))) == wb) then wb else if ((thirdField (fourthRow (gBoard vvv))) == ww) then ww else if ((thirdField (fourthRow (gBoard vvv))) == we) then we else be
                                   'D' -> if ((fourthField (fourthRow (gBoard vvv))) == bb) then bb else if ((fourthField (fourthRow (gBoard vvv))) == bw) then bw else if ((fourthField (fourthRow (gBoard vvv))) == wb) then wb else if ((fourthField (fourthRow (gBoard vvv))) == ww) then ww else if ((fourthField (fourthRow (gBoard vvv))) == we) then we else be
                                   _ -> bb
                            '1' -> case head (uuu) of 
                                   'A' -> if ((firstField (fifthRow (gBoard vvv))) == bb) then bb else if ((firstField (fifthRow (gBoard vvv))) == bw) then bw else if ((firstField (fifthRow (gBoard vvv))) == wb) then wb else if ((firstField (fifthRow (gBoard vvv))) == ww) then ww else if ((firstField (fifthRow (gBoard vvv))) == we) then we else be
                                   'B' -> if ((secondField (fifthRow (gBoard vvv))) == bb) then bb else if ((secondField (fifthRow (gBoard vvv))) == bw) then bw else if ((secondField (fifthRow (gBoard vvv))) == wb) then wb else if ((secondField (fifthRow (gBoard vvv))) == ww) then ww else if ((secondField (fifthRow (gBoard vvv))) == we) then we else be
                                   'C' -> if ((thirdField (fifthRow (gBoard vvv))) == bb) then bb else if ((thirdField (fifthRow (gBoard vvv))) == bw) then bw else if ((thirdField (fifthRow (gBoard vvv))) == wb) then wb else if ((thirdField (fifthRow (gBoard vvv))) == ww) then ww else if ((thirdField (fifthRow (gBoard vvv))) == we) then we else be
                                   'D' -> if ((fourthField (fifthRow (gBoard vvv))) == bb) then bb else if ((fourthField (fifthRow (gBoard vvv))) == bw) then bw else if ((fourthField (fifthRow (gBoard vvv))) == wb) then wb else if ((fourthField (fifthRow (gBoard vvv))) == ww) then ww else if ((fourthField (fifthRow (gBoard vvv))) == we) then we else be
                                   _ -> bb
                            _ -> bb

{--
updateRow :: String -> Field -> Row -> Row
updateRow ab cd ef = case head (ab) of
       'A' -> ((cd) : (secondField (ef)) : (thirdField (ef)) : (fourthField (ef)) : [])
       'B' -> ((firstField (ef)) : (cd) : (thirdField (ef)) : (fourthField (ef)) : [])
       'C' -> ((firstField (ef)) : (secondField (ef)) : (cd) : (fourthField (ef)) : [])
       'D' -> ((firstField (ef)) : (secondField (ef)) : (thirdField (ef)) : (cd) : [])

updateBoard :: String -> Row -> Board -> Board
updateBoard gh ji kl = case head (tail (gh)) of
       '5' -> ((ji) : (secondRow (kl)) : (thirdRow (kl)) : (fourthRow (kl)) : (fifthRow (kl)) : [])
       '4' -> ((firstRow (kl)) : (ji) : (thirdRow (kl)) : (fourthRow (kl)) : (fifthRow (kl)) : [])
       '3' -> ((firstRow (kl)) : (secondRow (kl)) : (ji) : (fourthRow (kl)) : (fifthRow (kl)) : [])
       '2' -> ((firstRow (kl)) : (secondRow (kl)) : (thirdRow (kl)) : (ji) : (fifthRow (kl)) : [])
       '1' -> ((firstRow (kl)) : (secondRow (kl)) : (thirdRow (kl)) : (fourthRow (kl)) : (ji) : [])
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


playMove :: World ()
playMove = do
    qqq1 <- get
    choice1 <- liftIO $ chooseBishop
    case ((evalInput1 choice1) == (Left "Invalid input.")) of 
       True -> do
              liftIO $ putStrLn "                                  "
              liftIO $ putStrLn "**********Error Message***********"
              liftIO $ putStrLn "Invalid input! Please, try again."
              liftIO $ putStrLn "**********************************"
              liftIO $ putStrLn "                                  "
              playMove
       False -> do
              case ((evalInput2 choice1 qqq1) == (Left "Invalid input.")) of 
                  True -> do
                     liftIO $ putStrLn "                                                    "
                     liftIO $ putStrLn "******************Error Message*********************"
                     liftIO $ putStrLn "There is no bishop on this field! Please, try again."
                     liftIO $ putStrLn "****************************************************"
                     liftIO $ putStrLn "                                                    "
                     playMove
                  False -> do
                     sss1 <- return (chosenField choice1 qqq1)
                     modify (\x -> (takeBishop choice1 qqq1))
                     qqq2 <- get
                     choice2 <- liftIO $ moveBishopTo
                     case ((evalInput1 choice2) == (Left "Invalid input.")) of 
                            True -> do
                                   liftIO $ putStrLn "                                  "
                                   liftIO $ putStrLn "**********Error Message***********"
                                   liftIO $ putStrLn "Invalid input! Please, try again."
                                   liftIO $ putStrLn "**********************************"
                                   liftIO $ putStrLn "                                  "
                                   modify (\x -> (qqq1))
                                   playMove
                            False -> do
                                   case ((evalInput3 choice2 qqq2) == (Left "Invalid input.")) of 
                                       True -> do
                                          liftIO $ putStrLn "                                                "
                                          liftIO $ putStrLn "*****************Error Message******************"
                                          liftIO $ putStrLn "Another Bishop on this field! Please, try again."
                                          liftIO $ putStrLn "************************************************"
                                          liftIO $ putStrLn "                                                "
                                          modify (\x -> (qqq1))
                                          playMove
                                       False -> do
                                          sss2 <- return (chosenField choice1 qqq2)
                                          modify (\x -> (positionBishop choice2 sss1 qqq2))
                                          qqq3 <- get
                                          sss3 <- return (chosenField choice2 qqq3)
                                          case ((evalInput4 choice2 sss3 qqq2) == (Left "Invalid input.")) of 
                                                 True -> do
                                                        liftIO $ putStrLn "                                                               "
                                                        liftIO $ putStrLn "************************Error Message**************************"
                                                        liftIO $ putStrLn "This field can't be accessed by this bishop! Please, try again."
                                                        liftIO $ putStrLn "***************************************************************"
                                                        liftIO $ putStrLn "                                                               "
                                                        modify (\x -> (qqq1))
                                                        playMove
                                                 False -> do
                                                        case ((evalInput5 sss1 sss2) == (Left "Invalid input.")) of 
                                                               True -> do
                                                                      liftIO $ putStrLn "                                                   "
                                                                      liftIO $ putStrLn "******************Error Message********************"
                                                                      liftIO $ putStrLn "A bishop can only move diagonal! Please, try again."
                                                                      liftIO $ putStrLn "***************************************************"
                                                                      liftIO $ putStrLn "                                                   "
                                                                      modify (\x -> (qqq1))
                                                                      playMove
                                                               False -> do

                                                                      modify (\x -> (positionBishop choice2 sss1 qqq3))
                                                                      qqq4 <- get
                                                                      --liftIO clearScreen
                                                                      liftIO $ putStrLn (printBoard (gBoard qqq4))
                                                                      case ((gBoard qqq3) == ([[bw, ww, bw, ww], [we, be, we, be], [be, we, be, we], [we, be, we, be], [bb, wb, bb, wb]])) of 
                                                                             True -> liftIO $ putStrLn "Congratulations. You did it!"
                                                                             False -> playMove
                                                                      --liftIO $ print (evalInput choice21)
                                                                      --print (evalInput2 choice21)   
                                                                      --print (evalInput choice22)
                                                                      --print (evalInput3 choice22)   
                                                                      --print (decideColorBishop choice1 game)

main :: IO ()
main = do
    setup
    game <- return (mkGame)
    runExceptT (runStateT playMove game)
    --putStrLn (printBoard (updateBoard choice1 (updateRow choice1 (chosenField choice2 game) (chosenRow choice2 game)) (gBoard game)))
    --runStateT (runExceptT playMove "error") game
    return ()

{--
playMove :: StateT Game IO ()
playMove = do
    qqq1 <- get
    choice1 <- liftIO $ chooseBishop
    sss <- return (chosenField choice1 qqq1)
    modify (\x -> (takeBishop choice1 qqq1))
    qqq2 <- get
    choice2 <- liftIO $ moveBishopTo
    modify (\x -> (positionBishop choice2 sss qqq2))
    qqq3 <- get
    --liftIO clearScreen
    liftIO $ putStrLn (printBoard (gBoard qqq3))
    case ((gBoard qqq3) == ([[bw, ww, bw, ww], [we, be, we, be], [be, we, be, we], [we, be, we, be], [bb, wb, bb, wb]])) of 
       True -> liftIO $ putStrLn "Congratulations. You did it!"
       False -> playMove
    --liftIO $ print (evalInput choice21)
    --print (evalInput2 choice21)   
    --print (evalInput choice22)
    --print (evalInput3 choice22)   
    --print (decideColorBishop choice1 game)

main :: IO ()
main = do
    setup
    game <- return (mkGame)
    runStateT playMove game
    --putStrLn (printBoard (updateBoard choice1 (updateRow choice1 (chosenField choice2 game) (chosenRow choice2 game)) (gBoard game)))
    --runStateT (runExceptT playMove "error") game
    return ()
--}
