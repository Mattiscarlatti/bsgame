module Main where

import Data.List ()
import Data.Char ( toUpper )
import Data.Either ()
import System.IO ()
import System.Console.ANSI ( clearScreen )
import Control.Monad.State ( State, StateT, runState, runStateT, liftIO, MonadState (get), put, modify )
import Control.Monad.Trans.Except ( ExceptT, runExceptT )

import View
import Evaluation

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

chosenRow :: String -> Game -> Row
chosenRow rt st = case head (tail (rt)) of
       '5' -> firstRow (gBoard st)
       '4' -> secondRow (gBoard st)
       '3' -> thirdRow (gBoard st)
       '2' -> fourthRow (gBoard st)
       '1' -> fifthRow (gBoard st)

chosenField :: String -> Row -> Field
chosenField uvw wvu = case head (uvw) of
       'A' -> if ((firstField (wvu)) == bb) then bb else if ((firstField (wvu)) == bw) then bw else if ((firstField (wvu)) == wb) then wb else if ((firstField (wvu)) == ww) then ww else if ((firstField (wvu)) == we) then we else be
       'B' -> if ((secondField (wvu)) == bb) then bb else if ((secondField (wvu)) == bw) then bw else if ((secondField (wvu)) == wb) then wb else if ((secondField (wvu)) == ww) then ww else if ((secondField (wvu)) == we) then we else be
       'C' -> if ((thirdField (wvu)) == bb) then bb else if ((thirdField (wvu)) == bw) then bw else if ((thirdField (wvu)) == wb) then wb else if ((thirdField (wvu)) == ww) then ww else if ((thirdField (wvu)) == we) then we else be
       'D' -> if ((fourthField (wvu)) == bb) then bb else if ((fourthField (wvu)) == bw) then bw else if ((fourthField (wvu)) == wb) then wb else if ((fourthField (wvu)) == ww) then ww else if ((fourthField (wvu)) == we) then we else be

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

-- Update board after taking a bishop away (to move)
takeBishop :: String -> Game -> Game
takeBishop ggh kkl = CGame { gBoard = (updateBoard ggh (updateRow ggh (if (chosenField ggh (chosenRow ggh kkl)) == bb || (chosenField ggh (chosenRow ggh kkl)) == bw then be else we) (chosenRow ggh kkl)) (gBoard kkl)), gScore = (gScore kkl) }

-- Update board after repositioning the selected bishop
positionBishop :: String -> Field -> Game -> Game
positionBishop uuuu vvvv ssss = CGame { gBoard = (updateBoard uuuu (updateRow uuuu vvvv (chosenRow uuuu ssss)) (gBoard ssss)), gScore = ((gScore ssss) + 1) }

printScore :: Game -> IO ()
printScore xzy = do
       score <- return ("                                            Moves used so far: " ++ (show (div (gScore xzy) 2)))
       putStrLn score
       putStrLn "                                                                 "


playMove :: World ()
playMove = do
    qqq1 <- get
    choice1 <- liftIO $ chooseBishop
    case ((evalInput1 choice1) == (Left "Invalid input.")) of 
       True -> do
              giveErrorMessage "Invalid input! Please, try again."
              liftIO $ putStrLn (printBoard (gBoard qqq1))
              liftIO $ printScore qqq1
              playMove
       False -> do
              case ((evalInput2 choice1 qqq1) == (Left "Invalid input.")) of 
                  True -> do
                     giveErrorMessage "There is no bishop on this field! Please, try again."
                     liftIO $ putStrLn (printBoard (gBoard qqq1))
                     liftIO $ printScore qqq1
                     playMove
                  False -> do
                     sss1 <- return (chosenField choice1 (chosenRow choice1 qqq1))
                     modify (\x -> (takeBishop choice1 qqq1))
                     qqq2 <- get
                     choice2 <- liftIO $ moveBishopTo
                     case ((evalInput1 choice2) == (Left "Invalid input.")) of 
                            True -> do
                                   giveErrorMessage "Invalid input! Please, try again."
                                   modify (\x -> (qqq1))
                                   liftIO $ putStrLn (printBoard (gBoard qqq1))
                                   liftIO $ printScore qqq1
                                   playMove
                            False -> do
                                   case ((evalInput3 choice2 qqq2) == (Left "Invalid input.")) of 
                                       True -> do
                                          giveErrorMessage "Another Bishop on this field! Please, try again."
                                          liftIO $ putStrLn (printBoard (gBoard qqq1))
                                          modify (\x -> (qqq1))
                                          liftIO $ printScore qqq1
                                          playMove
                                       False -> do
                                          sss2 <- return (chosenField choice1 (chosenRow choice1 qqq2))
                                          modify (\x -> (positionBishop choice2 sss1 qqq2))
                                          qqq3 <- get
                                          sss3 <- return (chosenField choice2 (chosenRow choice2 qqq3))
                                          case ((evalInput4 choice2 sss3 qqq2) == (Left "Invalid input.")) of 
                                                 True -> do
                                                        giveErrorMessage "This field can't be accessed by this bishop! Please, try again."
                                                        modify (\x -> (qqq1))
                                                        liftIO $ putStrLn (printBoard (gBoard qqq1))
                                                        liftIO $ printScore qqq1
                                                        playMove
                                                 False -> do
                                                        case ((evalInput5 choice1 choice2) == (Left "Invalid input.")) of 
                                                               True -> do
                                                                      giveErrorMessage "A bishop can only move diagonal! Please, try again."
                                                                      modify (\x -> (qqq1))
                                                                      liftIO $ putStrLn (printBoard (gBoard qqq1))
                                                                      liftIO $ printScore qqq1
                                                                      playMove
                                                               False -> do
                                                                      modify (\x -> (positionBishop choice2 sss1 qqq3))
                                                                      qqq4 <- get
                                                                      liftIO clearScreen
                                                                      liftIO $ putStrLn (printBoard (gBoard qqq4))
                                                                      liftIO $ printScore qqq4
                                                                      case ((gBoard qqq3) == ([[bw, ww, bw, ww], [we, be, we, be], [be, we, be, we], [we, be, we, be], [bb, wb, bb, wb]])) of 
                                                                             True -> liftIO $ putStrLn "Congratulations. You did it!"
                                                                             False -> playMove

main :: IO ()
main = do
    setup
    game <- return (mkGame)
    runExceptT (runStateT playMove game)
    return ()
