module Main where

import Data.List ()
import Data.Char ( toUpper )
import Data.Either ()
import System.IO ()
import System.Console.ANSI ( clearScreen )
import Control.Monad.State ( State, StateT, runState, runStateT, liftIO, MonadState (get), modify )
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

decideColorBishop :: String -> Game -> Field
decideColorBishop uuu vvv = case head (tail (uuu)) of
                            '1' -> case head (uuu) of 
                                   'A' -> if ((firstField (fifthRow (gBoard vvv))) == bb) then bb else if ((firstField (fifthRow (gBoard vvv))) == bw) then bw else if ((firstField (fifthRow (gBoard vvv))) == wb) then wb else ww
                                   'B' -> if ((secondField (fifthRow (gBoard vvv))) == bb) then bb else if ((secondField (fifthRow (gBoard vvv))) == bw) then bw else if ((secondField (fifthRow (gBoard vvv))) == wb) then wb else ww
                                   'C' -> if ((thirdField (fifthRow (gBoard vvv))) == bb) then bb else if ((thirdField (fifthRow (gBoard vvv))) == bw) then bw else if ((thirdField (fifthRow (gBoard vvv))) == wb) then wb else ww
                                   'D' -> if ((fourthField (fifthRow (gBoard vvv))) == bb) then bb else if ((fourthField (fifthRow (gBoard vvv))) == bw) then bw else if ((fourthField (fifthRow (gBoard vvv))) == wb) then wb else ww
                            '2' -> case head (uuu) of 
                                   'A' -> if ((firstField (fourthRow (gBoard vvv))) == bb) then bb else if ((firstField (fourthRow (gBoard vvv))) == bw) then bw else if ((firstField (fourthRow (gBoard vvv))) == wb) then wb else ww
                                   'B' -> if ((secondField (fourthRow (gBoard vvv))) == bb) then bb else if ((secondField (fourthRow (gBoard vvv))) == bw) then bw else if ((secondField (fourthRow (gBoard vvv))) == wb) then wb else ww
                                   'C' -> if ((thirdField (fourthRow (gBoard vvv))) == bb) then bb else if ((thirdField (fourthRow (gBoard vvv))) == bw) then bw else if ((thirdField (fourthRow (gBoard vvv))) == wb) then wb else ww
                                   'D' -> if ((fourthField (fourthRow (gBoard vvv))) == bb) then bb else if ((fourthField (fourthRow (gBoard vvv))) == bw) then bw else if ((fourthField (fourthRow (gBoard vvv))) == wb) then wb else ww
                            '3' -> case head (uuu) of 
                                   'A' -> if ((firstField (thirdRow (gBoard vvv))) == bb) then bb else if ((firstField (thirdRow (gBoard vvv))) == bw) then bw else if ((firstField (thirdRow (gBoard vvv))) == wb) then wb else ww
                                   'B' -> if ((secondField (thirdRow (gBoard vvv))) == bb) then bb else if ((secondField (thirdRow (gBoard vvv))) == bw) then bw else if ((secondField (thirdRow (gBoard vvv))) == wb) then wb else ww
                                   'C' -> if ((thirdField (thirdRow (gBoard vvv))) == bb) then bb else if ((thirdField (thirdRow (gBoard vvv))) == bw) then bw else if ((thirdField (thirdRow (gBoard vvv))) == wb) then wb else ww
                                   'D' -> if ((fourthField (thirdRow (gBoard vvv))) == bb) then bb else if ((fourthField (thirdRow (gBoard vvv))) == bw) then bw else if ((fourthField (thirdRow (gBoard vvv))) == wb) then wb else ww
                            '4' -> case head (uuu) of 
                                   'A' -> if ((firstField (secondRow (gBoard vvv))) == bb) then bb else if ((firstField (secondRow (gBoard vvv))) == bw) then bw else if ((firstField (secondRow (gBoard vvv))) == wb) then wb else ww
                                   'B' -> if ((secondField (secondRow (gBoard vvv))) == bb) then bb else if ((secondField (secondRow (gBoard vvv))) == bw) then bw else if ((secondField (secondRow (gBoard vvv))) == wb) then wb else ww
                                   'C' -> if ((thirdField (secondRow (gBoard vvv))) == bb) then bb else if ((thirdField (secondRow (gBoard vvv))) == bw) then bw else if ((thirdField (secondRow (gBoard vvv))) == wb) then wb else ww
                                   'D' -> if ((fourthField (secondRow (gBoard vvv))) == bb) then bb else if ((fourthField (secondRow (gBoard vvv))) == bw) then bw else if ((fourthField (secondRow (gBoard vvv))) == wb) then wb else ww
                            '5' -> case head (uuu) of 
                                   'A' -> if ((firstField (firstRow (gBoard vvv))) == bb) then bb else if ((firstField (firstRow (gBoard vvv))) == bw) then bw else if ((firstField (firstRow (gBoard vvv))) == wb) then wb else ww
                                   'B' -> if ((secondField (firstRow (gBoard vvv))) == bb) then bb else if ((secondField (firstRow (gBoard vvv))) == bw) then bw else if ((secondField (firstRow (gBoard vvv))) == wb) then wb else ww
                                   'C' -> if ((thirdField (firstRow (gBoard vvv))) == bb) then bb else if ((thirdField (firstRow (gBoard vvv))) == bw) then bw else if ((thirdField (firstRow (gBoard vvv))) == wb) then wb else ww
                                   'D' -> if ((fourthField (firstRow (gBoard vvv))) == bb) then bb else if ((fourthField (firstRow (gBoard vvv))) == bw) then bw else if ((fourthField (firstRow (gBoard vvv))) == wb) then wb else ww

takeBishop :: String -> State Game ()
takeBishop uu = do 
    xx <- get
    case head (tail (uu)) of 
                            '5' -> case head (uu) of 
                                   'A' -> modify (\x -> (CGame { gBoard = (((be) : (((gBoard xx) !! 0) !! 1) : (((gBoard xx) !! 0) !! 2) : (((gBoard xx) !! 0) !! 3) : []) : ((gBoard xx) !! 1) : ((gBoard xx) !! 2) : ((gBoard xx) !! 3) : ((gBoard xx) !! 4) : []) }))
                                   'B' -> modify (\x -> (CGame { gBoard = (((((gBoard xx) !! 0) !! 0) : (we) : (((gBoard xx) !! 0) !! 2) : (((gBoard xx) !! 0) !! 3) : []) : ((gBoard xx) !! 1) : ((gBoard xx) !! 2) : ((gBoard xx) !! 3) : ((gBoard xx) !! 4) : []) }))
                                   'C' -> modify (\x -> (CGame { gBoard = (((((gBoard xx) !! 0) !! 0) : (((gBoard xx) !! 0) !! 1) : (be) : (((gBoard xx) !! 0) !! 3) : []) : ((gBoard xx) !! 1) : ((gBoard xx) !! 2) : ((gBoard xx) !! 3) : ((gBoard xx) !! 4) : []) }))
                                   'D' -> modify (\x -> (CGame { gBoard = (((((gBoard xx) !! 0) !! 0) : (((gBoard xx) !! 0) !! 1) : (((gBoard xx) !! 0) !! 2) : (we) : []) : ((gBoard xx) !! 1) : ((gBoard xx) !! 2) : ((gBoard xx) !! 3) : ((gBoard xx) !! 4) : []) }))
                            '4' -> case head (uu) of 
                                   'A' -> modify (\x -> (CGame { gBoard = (((gBoard xx) !! 0) : ((we) : (((gBoard xx) !! 1) !! 1) : (((gBoard xx) !! 1) !! 2) : (((gBoard xx) !! 1) !! 3) : []) : ((gBoard xx) !! 2) : ((gBoard xx) !! 3) : ((gBoard xx) !! 4) : []) }))
                                   'B' -> modify (\x -> (CGame { gBoard = (((gBoard xx) !! 0) : ((((gBoard xx) !! 1) !! 0) : (be) : (((gBoard xx) !! 1) !! 2) : (((gBoard xx) !! 1) !! 3) : []) : ((gBoard xx) !! 2) : ((gBoard xx) !! 3) : ((gBoard xx) !! 4) : []) }))
                                   'C' -> modify (\x -> (CGame { gBoard = (((gBoard xx) !! 0) : ((((gBoard xx) !! 1) !! 0) : (((gBoard xx) !! 1) !! 1) : (we) : (((gBoard xx) !! 1) !! 3) : []) : ((gBoard xx) !! 2) : ((gBoard xx) !! 3) : ((gBoard xx) !! 4) : []) }))
                                   'D' -> modify (\x -> (CGame { gBoard = (((gBoard xx) !! 0) : ((((gBoard xx) !! 1) !! 0) : (((gBoard xx) !! 1) !! 1) : (((gBoard xx) !! 1) !! 2) : (be) : []) : ((gBoard xx) !! 2) : ((gBoard xx) !! 3) : ((gBoard xx) !! 4) : []) }))
                            '3' -> case head (uu) of 
                                   'A' -> modify (\x -> (CGame { gBoard = (((gBoard xx) !! 0) : ((gBoard xx) !! 1) : ((be) : (((gBoard xx) !! 2) !! 1) : (((gBoard xx) !! 2) !! 2) : (((gBoard xx) !! 2) !! 3) : []) : ((gBoard xx) !! 3) : ((gBoard xx) !! 4) : []) }))
                                   'B' -> modify (\x -> (CGame { gBoard = (((gBoard xx) !! 0) : ((gBoard xx) !! 1) : ((((gBoard xx) !! 2) !! 0) : (we) : (((gBoard xx) !! 2) !! 2) : (((gBoard xx) !! 2) !! 3) : []) : ((gBoard xx) !! 3) : ((gBoard xx) !! 4) : []) }))
                                   'C' -> modify (\x -> (CGame { gBoard = (((gBoard xx) !! 0) : ((gBoard xx) !! 1) : ((((gBoard xx) !! 2) !! 0) : (((gBoard xx) !! 2) !! 1) : (be) : (((gBoard xx) !! 2) !! 3) : []) : ((gBoard xx) !! 3) : ((gBoard xx) !! 4) : []) }))
                                   'D' -> modify (\x -> (CGame { gBoard = (((gBoard xx) !! 0) : ((gBoard xx) !! 1) : ((((gBoard xx) !! 2) !! 0) : (((gBoard xx) !! 2) !! 1) : (((gBoard xx) !! 2) !! 2) : (we) : []) : ((gBoard xx) !! 3) : ((gBoard xx) !! 4) : []) }))
                            '2' -> case head (uu) of 
                                   'A' -> modify (\x -> (CGame { gBoard = (((gBoard xx) !! 0) : ((gBoard xx) !! 1) : ((gBoard xx) !! 2) : ((we) : (((gBoard xx) !! 3) !! 1) : (((gBoard xx) !! 3) !! 2) : (((gBoard xx) !! 3) !! 3) : []) : ((gBoard xx) !! 4) : []) }))
                                   'B' -> modify (\x -> (CGame { gBoard = (((gBoard xx) !! 0) : ((gBoard xx) !! 1) : ((gBoard xx) !! 2) : ((((gBoard xx) !! 3) !! 0) : (be) : (((gBoard xx) !! 3) !! 2) : (((gBoard xx) !! 3) !! 3) : []) : ((gBoard xx) !! 4) : []) }))
                                   'C' -> modify (\x -> (CGame { gBoard = (((gBoard xx) !! 0) : ((gBoard xx) !! 1) : ((gBoard xx) !! 2) : ((((gBoard xx) !! 3) !! 0) : (((gBoard xx) !! 3) !! 1) : (we) : (((gBoard xx) !! 3) !! 3) : []) : ((gBoard xx) !! 4) : []) }))
                            '1' -> case head (uu) of 
                                   'A' -> modify (\x -> (CGame { gBoard = (((gBoard xx) !! 0) : ((gBoard xx) !! 1) : ((gBoard xx) !! 2) : ((gBoard xx) !! 3) : ((be) : (((gBoard xx) !! 4) !! 1) : (((gBoard xx) !! 4) !! 2) : (((gBoard xx) !! 4) !! 3) : []) : []) }))
                                   'B' -> modify (\x -> (CGame { gBoard = (((gBoard xx) !! 0) : ((gBoard xx) !! 1) : ((gBoard xx) !! 2) : ((gBoard xx) !! 3) : ((((gBoard xx) !! 4) !! 0) : (we) : (((gBoard xx) !! 4) !! 2) : (((gBoard xx) !! 4) !! 3) : []) : []) }))
                                   'C' -> modify (\x -> (CGame { gBoard = (((gBoard xx) !! 0) : ((gBoard xx) !! 1) : ((gBoard xx) !! 2) : ((gBoard xx) !! 3) : ((((gBoard xx) !! 4) !! 0) : (((gBoard xx) !! 4) !! 1) : (be) : (((gBoard xx) !! 4) !! 3) : []) : []) }))
                                   'D' -> modify (\x -> (CGame { gBoard = (((gBoard xx) !! 0) : ((gBoard xx) !! 1) : ((gBoard xx) !! 2) : ((gBoard xx) !! 3) : ((((gBoard xx) !! 4) !! 0) : (((gBoard xx) !! 4) !! 1) : (((gBoard xx) !! 4) !! 2) : (we) : []) : []) }))

positionBishop :: String -> String -> State Game ()
positionBishop uuuu vv = do 
    yy <- get
    zz <- return (decideColorBishop uuuu yy)
    case head (tail (vv)) of 
                            '5' -> case head (vv) of 
                                   'A' -> modify (\y -> (CGame { gBoard = (((zz) : (((gBoard yy) !! 0) !! 1) : (((gBoard yy) !! 0) !! 2) : (((gBoard yy) !! 0) !! 3) : []) : ((gBoard yy) !! 1) : ((gBoard yy) !! 2) : ((gBoard yy) !! 3) : ((gBoard yy) !! 4) : []) }))
                                   'B' -> modify (\y -> (CGame { gBoard = (((((gBoard yy) !! 0) !! 0) : (zz) : (((gBoard yy) !! 0) !! 2) : (((gBoard yy) !! 0) !! 3) : []) : ((gBoard yy) !! 1) : ((gBoard yy) !! 2) : ((gBoard yy) !! 3) : ((gBoard yy) !! 4) : []) }))
                                   'C' -> modify (\y -> (CGame { gBoard = (((((gBoard yy) !! 0) !! 0) : (((gBoard yy) !! 0) !! 1) : (zz) : (((gBoard yy) !! 0) !! 3) : []) : ((gBoard yy) !! 1) : ((gBoard yy) !! 2) : ((gBoard yy) !! 3) : ((gBoard yy) !! 4) : []) }))
                                   'D' -> modify (\y -> (CGame { gBoard = (((((gBoard yy) !! 0) !! 0) : (((gBoard yy) !! 0) !! 1) : (((gBoard yy) !! 0) !! 2) : (zz) : []) : ((gBoard yy) !! 1) : ((gBoard yy) !! 2) : ((gBoard yy) !! 3) : ((gBoard yy) !! 4) : []) }))
                            '4' -> case head (vv) of 
                                   'A' -> modify (\y -> (CGame { gBoard = (((gBoard yy) !! 0) : ((zz) : (((gBoard yy) !! 1) !! 1) : (((gBoard yy) !! 1) !! 2) : (((gBoard yy) !! 1) !! 3) : []) : ((gBoard yy) !! 2) : ((gBoard yy) !! 3) : ((gBoard yy) !! 4) : []) }))
                                   'B' -> modify (\y -> (CGame { gBoard = (((gBoard yy) !! 0) : ((((gBoard yy) !! 1) !! 0) : (zz) : (((gBoard yy) !! 1) !! 2) : (((gBoard yy) !! 1) !! 3) : []) : ((gBoard yy) !! 2) : ((gBoard yy) !! 3) : ((gBoard yy) !! 4) : []) }))
                                   'C' -> modify (\y -> (CGame { gBoard = (((gBoard yy) !! 0) : ((((gBoard yy) !! 1) !! 0) : (((gBoard yy) !! 1) !! 1) : (zz) : (((gBoard yy) !! 1) !! 3) : []) : ((gBoard yy) !! 2) : ((gBoard yy) !! 3) : ((gBoard yy) !! 4) : []) }))
                                   'D' -> modify (\y -> (CGame { gBoard = (((gBoard yy) !! 0) : ((((gBoard yy) !! 1) !! 0) : (((gBoard yy) !! 1) !! 1) : (((gBoard yy) !! 1) !! 2) : (zz) : []) : ((gBoard yy) !! 2) : ((gBoard yy) !! 3) : ((gBoard yy) !! 4) : []) }))
                            '3' -> case head (vv) of 
                                   'A' -> modify (\y -> (CGame { gBoard = (((gBoard yy) !! 0) : ((gBoard yy) !! 1) : ((zz) : (((gBoard yy) !! 2) !! 1) : (((gBoard yy) !! 2) !! 2) : (((gBoard yy) !! 2) !! 3) : []) : ((gBoard yy) !! 3) : ((gBoard yy) !! 4) : []) }))
                                   'B' -> modify (\y -> (CGame { gBoard = (((gBoard yy) !! 0) : ((gBoard yy) !! 1) : ((((gBoard yy) !! 2) !! 0) : (zz) : (((gBoard yy) !! 2) !! 2) : (((gBoard yy) !! 2) !! 3) : []) : ((gBoard yy) !! 3) : ((gBoard yy) !! 4) : []) }))
                                   'C' -> modify (\y -> (CGame { gBoard = (((gBoard yy) !! 0) : ((gBoard yy) !! 1) : ((((gBoard yy) !! 2) !! 0) : (((gBoard yy) !! 2) !! 1) : (zz) : (((gBoard yy) !! 2) !! 3) : []) : ((gBoard yy) !! 3) : ((gBoard yy) !! 4) : []) }))
                                   'D' -> modify (\y -> (CGame { gBoard = (((gBoard yy) !! 0) : ((gBoard yy) !! 1) : ((((gBoard yy) !! 2) !! 0) : (((gBoard yy) !! 2) !! 1) : (((gBoard yy) !! 2) !! 2) : (zz) : []) : ((gBoard yy) !! 3) : ((gBoard yy) !! 4) : []) }))
                            '2' -> case head (vv) of 
                                   'A' -> modify (\y -> (CGame { gBoard = (((gBoard yy) !! 0) : ((gBoard yy) !! 1) : ((gBoard yy) !! 2) : ((zz) : (((gBoard yy) !! 3) !! 1) : (((gBoard yy) !! 3) !! 2) : (((gBoard yy) !! 3) !! 3) : []) : ((gBoard yy) !! 4) : []) }))
                                   'B' -> modify (\y -> (CGame { gBoard = (((gBoard yy) !! 0) : ((gBoard yy) !! 1) : ((gBoard yy) !! 2) : ((((gBoard yy) !! 3) !! 0) : (zz) : (((gBoard yy) !! 3) !! 2) : (((gBoard yy) !! 3) !! 3) : []) : ((gBoard yy) !! 4) : []) }))
                                   'C' -> modify (\y -> (CGame { gBoard = (((gBoard yy) !! 0) : ((gBoard yy) !! 1) : ((gBoard yy) !! 2) : ((((gBoard yy) !! 3) !! 0) : (((gBoard yy) !! 3) !! 1) : (zz) : (((gBoard yy) !! 3) !! 3) : []) : ((gBoard yy) !! 4) : []) }))
                                   'D' -> modify (\y -> (CGame { gBoard = (((gBoard yy) !! 0) : ((gBoard yy) !! 1) : ((gBoard yy) !! 2) : ((((gBoard yy) !! 3) !! 0) : (((gBoard yy) !! 3) !! 1) : (((gBoard yy) !! 3) !! 2) : (zz) : []) : ((gBoard yy) !! 4) : []) }))
                            '1' -> case head (vv) of 
                                   'A' -> modify (\y -> (CGame { gBoard = (((gBoard yy) !! 0) : ((gBoard yy) !! 1) : ((gBoard yy) !! 2) : ((gBoard yy) !! 3) : ((zz) : (((gBoard yy) !! 4) !! 1) : (((gBoard yy) !! 4) !! 2) : (((gBoard yy) !! 4) !! 3) : []) : []) }))
                                   'B' -> modify (\y -> (CGame { gBoard = (((gBoard yy) !! 0) : ((gBoard yy) !! 1) : ((gBoard yy) !! 2) : ((gBoard yy) !! 3) : ((((gBoard yy) !! 4) !! 0) : (zz) : (((gBoard yy) !! 4) !! 2) : (((gBoard yy) !! 4) !! 3) : []) : []) }))
                                   'C' -> modify (\y -> (CGame { gBoard = (((gBoard yy) !! 0) : ((gBoard yy) !! 1) : ((gBoard yy) !! 2) : ((gBoard yy) !! 3) : ((((gBoard yy) !! 4) !! 0) : (((gBoard yy) !! 4) !! 1) : (zz) : (((gBoard yy) !! 4) !! 3) : []) : []) }))
                                   'D' -> modify (\y -> (CGame { gBoard = (((gBoard yy) !! 0) : ((gBoard yy) !! 1) : ((gBoard yy) !! 2) : ((gBoard yy) !! 3) : ((((gBoard yy) !! 4) !! 0) : (((gBoard yy) !! 4) !! 1) : (((gBoard yy) !! 4) !! 2) : (zz) : []) : []) }))
    return ()

state :: String -> String -> State Game ()
state aaaa bbbb = do 
    takeBishop aaaa
    positionBishop aaaa bbbb

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

playMove :: String -> String -> Game -> StateT Game IO ()
playMove o p q = do
    newBoard <- return (gBoard (snd (runState (state o p) q)))
    liftIO $ putStrLn (printBoard (newBoard))
    qqq <- get
    liftIO $ do 
       choice21 <- chooseBishop
       choice22 <- moveBishopTo
       print (evalInput choice21)
       playMove choice21 choice22 (snd (runState (state choice21 choice22) qqq))
--    print (evalInput2 choice21)   
--    print (evalInput choice22)
--    print (evalInput3 choice22)   
--    print (decideColorBishop choice1 game)

main :: IO ()
main = do
    setup
    game <- return (mkGame)
    choice11 <- chooseBishop
    choice12 <- moveBishopTo
    return (snd (runState (state choice11 choice12) game))
    playMove choice11 choice12 game
    --state <- return (firstField (firstRow (gBoard game)))
    --print (state)
    --runStateT (runExceptT playMove "error") game
    return ()

