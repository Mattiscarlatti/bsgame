module View where

import System.Console.ANSI ( clearScreen )

data Field = CField { gp1 :: String, gp2 :: String, gp3 :: String, gp4 :: String, gp5 :: String, gp6 :: String, gp7 :: String }
  deriving (Eq, Show)

type Row = [Field]
type Column = [Field]
type Board = [Row]

data Game = CGame { gBoard :: Board }
  deriving (Eq, Show)

bb :: Field
bb = CField "||||  __  |||||" 
            "|||  (XX)  ||||" 
            "||   <XX>   |||" 
            "||   |XX|   |||" 
            "|||  /XX   ||||" 
            "||||      |||||" 
            "-------------- "
bw :: Field
bw = CField "||||  __  |||||" "|||  (  )  ||||" "||   <  >   |||" "||   |  |   |||" "|||  /__   ||||" "||||      |||||" "-------------- "
be :: Field
be = CField "|||||||||||||||" "|||||||||||||||" "|||||||||||||||" "|||||||||||||||" "|||||||||||||||" "|||||||||||||||" "-------------- "
ww :: Field
ww = CField "      __      |" "     (  )     |" "     <  >     |" "     |  |     |" "     /__      |" "              |" "-------------- "
wb :: Field
wb = CField "      __      |" "     (XX)     |" "     <XX>     |" "     |XX|     |" "     /XX      |" "              |" "-------------- "
we :: Field
we = CField "              |" "              |" "              |" "              |" "              |" "              |" "-------------- "

_StartBoard :: Board
_StartBoard = [[bb, wb, bb, wb], 
               [we, be, we, be], 
               [be, we, be, we], 
               [we, be, we, be], 
               [bw, ww, bw, ww]]

mkGame :: Game
mkGame = CGame { gBoard = _StartBoard }

firstRow :: Board -> Row
firstRow a = head (a)
secondRow :: Board -> Row
secondRow b = head (tail (b))
thirdRow :: Board -> Row
thirdRow c = head (tail (tail (c)))
fourthRow :: Board -> Row
fourthRow d = head (tail (tail (tail (d))))
fifthRow :: Board -> Row
fifthRow e = head (tail (tail (tail (tail (e)))))

firstField :: Row -> Field
firstField f = head (f)
secondField :: Row -> Field
secondField g = head (tail (g))
thirdField :: Row -> Field
thirdField h = head (tail (tail (h)))
fourthField :: Row -> Field
fourthField i = head (tail (tail (tail (i))))

printBoard :: Board -> String
printBoard zzz = do
    unlines ["                                                                 ",
             "     ----------------------------------------------------------- ",
             ("    |" ++ (gp1 (firstField (firstRow zzz))) ++ (gp1 (secondField (firstRow zzz))) ++ (gp1 (thirdField (firstRow zzz))) ++ (gp1 (fourthField (firstRow zzz)))),
             ("    |" ++ (gp2 (firstField (firstRow zzz))) ++ (gp2 (secondField (firstRow zzz))) ++ (gp2 (thirdField (firstRow zzz))) ++ (gp2 (fourthField (firstRow zzz)))), 
             ("    |" ++ (gp3 (firstField (firstRow zzz))) ++ (gp3 (secondField (firstRow zzz))) ++ (gp3 (thirdField (firstRow zzz))) ++ (gp3 (fourthField (firstRow zzz)))), 
             (" 5  |" ++ (gp4 (firstField (firstRow zzz))) ++ (gp4 (secondField (firstRow zzz))) ++ (gp4 (thirdField (firstRow zzz))) ++ (gp4 (fourthField (firstRow zzz)))), 
             ("    |" ++ (gp5 (firstField (firstRow zzz))) ++ (gp5 (secondField (firstRow zzz))) ++ (gp5 (thirdField (firstRow zzz))) ++ (gp5 (fourthField (firstRow zzz)))), 
             ("    |" ++ (gp6 (firstField (firstRow zzz))) ++ (gp6 (secondField (firstRow zzz))) ++ (gp6 (thirdField (firstRow zzz))) ++ (gp6 (fourthField (firstRow zzz)))), 
             ("     " ++ (gp7 (firstField (firstRow zzz))) ++ (gp7 (secondField (firstRow zzz))) ++ (gp7 (thirdField (firstRow zzz))) ++ (gp7 (fourthField (firstRow zzz)))),
             ("    |" ++ (gp1 (firstField (secondRow zzz))) ++ (gp1 (secondField (secondRow zzz))) ++ (gp1 (thirdField (secondRow zzz))) ++ (gp1 (fourthField (secondRow zzz)))),
             ("    |" ++ (gp2 (firstField (secondRow zzz))) ++ (gp2 (secondField (secondRow zzz))) ++ (gp2 (thirdField (secondRow zzz))) ++ (gp2 (fourthField (secondRow zzz)))), 
             ("    |" ++ (gp3 (firstField (secondRow zzz))) ++ (gp3 (secondField (secondRow zzz))) ++ (gp3 (thirdField (secondRow zzz))) ++ (gp3 (fourthField (secondRow zzz)))), 
             (" 4  |" ++ (gp4 (firstField (secondRow zzz))) ++ (gp4 (secondField (secondRow zzz))) ++ (gp4 (thirdField (secondRow zzz))) ++ (gp4 (fourthField (secondRow zzz)))), 
             ("    |" ++ (gp5 (firstField (secondRow zzz))) ++ (gp5 (secondField (secondRow zzz))) ++ (gp5 (thirdField (secondRow zzz))) ++ (gp5 (fourthField (secondRow zzz)))), 
             ("    |" ++ (gp6 (firstField (secondRow zzz))) ++ (gp6 (secondField (secondRow zzz))) ++ (gp6 (thirdField (secondRow zzz))) ++ (gp6 (fourthField (secondRow zzz)))), 
             ("     " ++ (gp7 (firstField (secondRow zzz))) ++ (gp7 (secondField (secondRow zzz))) ++ (gp7 (thirdField (secondRow zzz))) ++ (gp7 (fourthField (secondRow zzz)))),
             ("    |" ++ (gp1 (firstField (thirdRow zzz))) ++ (gp1 (secondField (thirdRow zzz))) ++ (gp1 (thirdField (thirdRow zzz))) ++ (gp1 (fourthField (thirdRow zzz)))),
             ("    |" ++ (gp2 (firstField (thirdRow zzz))) ++ (gp2 (secondField (thirdRow zzz))) ++ (gp2 (thirdField (thirdRow zzz))) ++ (gp2 (fourthField (thirdRow zzz)))), 
             ("    |" ++ (gp3 (firstField (thirdRow zzz))) ++ (gp3 (secondField (thirdRow zzz))) ++ (gp3 (thirdField (thirdRow zzz))) ++ (gp3 (fourthField (thirdRow zzz)))), 
             (" 3  |" ++ (gp4 (firstField (thirdRow zzz))) ++ (gp4 (secondField (thirdRow zzz))) ++ (gp4 (thirdField (thirdRow zzz))) ++ (gp4 (fourthField (thirdRow zzz)))), 
             ("    |" ++ (gp5 (firstField (thirdRow zzz))) ++ (gp5 (secondField (thirdRow zzz))) ++ (gp5 (thirdField (thirdRow zzz))) ++ (gp5 (fourthField (thirdRow zzz)))), 
             ("    |" ++ (gp6 (firstField (thirdRow zzz))) ++ (gp6 (secondField (thirdRow zzz))) ++ (gp6 (thirdField (thirdRow zzz))) ++ (gp6 (fourthField (thirdRow zzz)))), 
             ("     " ++ (gp7 (firstField (thirdRow zzz))) ++ (gp7 (secondField (thirdRow zzz))) ++ (gp7 (thirdField (thirdRow zzz))) ++ (gp7 (fourthField (thirdRow zzz)))),
             ("    |" ++ (gp1 (firstField (fourthRow zzz))) ++ (gp1 (secondField (fourthRow zzz))) ++ (gp1 (thirdField (fourthRow zzz))) ++ (gp1 (fourthField (fourthRow zzz)))),
             ("    |" ++ (gp2 (firstField (fourthRow zzz))) ++ (gp2 (secondField (fourthRow zzz))) ++ (gp2 (thirdField (fourthRow zzz))) ++ (gp2 (fourthField (fourthRow zzz)))), 
             ("    |" ++ (gp3 (firstField (fourthRow zzz))) ++ (gp3 (secondField (fourthRow zzz))) ++ (gp3 (thirdField (fourthRow zzz))) ++ (gp3 (fourthField (fourthRow zzz)))), 
             (" 2  |" ++ (gp4 (firstField (fourthRow zzz))) ++ (gp4 (secondField (fourthRow zzz))) ++ (gp4 (thirdField (fourthRow zzz))) ++ (gp4 (fourthField (fourthRow zzz)))), 
             ("    |" ++ (gp5 (firstField (fourthRow zzz))) ++ (gp5 (secondField (fourthRow zzz))) ++ (gp5 (thirdField (fourthRow zzz))) ++ (gp5 (fourthField (fourthRow zzz)))), 
             ("    |" ++ (gp6 (firstField (fourthRow zzz))) ++ (gp6 (secondField (fourthRow zzz))) ++ (gp6 (thirdField (fourthRow zzz))) ++ (gp6 (fourthField (fourthRow zzz)))), 
             ("     " ++ (gp7 (firstField (fourthRow zzz))) ++ (gp7 (secondField (fourthRow zzz))) ++ (gp7 (thirdField (fourthRow zzz))) ++ (gp7 (fourthField (fourthRow zzz)))),
             ("    |" ++ (gp1 (firstField (fifthRow zzz))) ++ (gp1 (secondField (fifthRow zzz))) ++ (gp1 (thirdField (fifthRow zzz))) ++ (gp1 (fourthField (fifthRow zzz)))),
             ("    |" ++ (gp2 (firstField (fifthRow zzz))) ++ (gp2 (secondField (fifthRow zzz))) ++ (gp2 (thirdField (fifthRow zzz))) ++ (gp2 (fourthField (fifthRow zzz)))), 
             ("    |" ++ (gp3 (firstField (fifthRow zzz))) ++ (gp3 (secondField (fifthRow zzz))) ++ (gp3 (thirdField (fifthRow zzz))) ++ (gp3 (fourthField (fifthRow zzz)))), 
             (" 1  |" ++ (gp4 (firstField (fifthRow zzz))) ++ (gp4 (secondField (fifthRow zzz))) ++ (gp4 (thirdField (fifthRow zzz))) ++ (gp4 (fourthField (fifthRow zzz)))), 
             ("    |" ++ (gp5 (firstField (fifthRow zzz))) ++ (gp5 (secondField (fifthRow zzz))) ++ (gp5 (thirdField (fifthRow zzz))) ++ (gp5 (fourthField (fifthRow zzz)))), 
             ("    |" ++ (gp6 (firstField (fifthRow zzz))) ++ (gp6 (secondField (fifthRow zzz))) ++ (gp6 (thirdField (fifthRow zzz))) ++ (gp6 (fourthField (fifthRow zzz)))), 
             ("     " ++ (gp7 (firstField (fifthRow zzz))) ++ (gp7 (secondField (fifthRow zzz))) ++ (gp7 (thirdField (fifthRow zzz))) ++ (gp7 (fourthField (fifthRow zzz)))),
             "                                                                 ", 
             "            A              B              C              D       ", 
             "                                                                 ",
             "                                                                 "]

setup :: IO ()
setup = do 
    clearScreen
    putStrLn ("                                                                 ")
    putStrLn ("              Welcome, Player, to the BISHOP SWAP GAME!          ")
    putStrLn (printBoard _StartBoard)
    putStrLn ("Goal of the game: swap all bishops. In as few moves as possible!")
    putStrLn ("Bishops can't enter fields covered by the opposite color.")
    putStrLn ("Here you go!")
    putStrLn ("                                                                 ")