-- Yunyi Ding yding13@ucsc.edu
-- Brian Lin bjlin@ucsc.edu

import Data.List.Split
import System.Exit
import System.IO
import System.Random
import System.Time

data GameState = GameState {
    playerLoc :: (Int, Int),
    playerHunger :: Int,
    playerHealth :: Int,
    inventory :: [Item],
    turns :: Int,
    environment :: [[Cell]]
    }

main = do writeFile "tse.tmp" ""
          genEnvironment mapWidth mapHeight
          start <- randomRIO (1,5)
          startMessage start
          putStr "You realize you're in the middle of the wilderness.\n"
          map <- readFile "tse.tmp"
          let gs = GameState (27, 12) 100 100 [] 0 (readEnvironment map mapWidth mapHeight)
          let gs' = see gs
          printMap gs' mapWidth mapHeight
          putStr "Type \"help\" to view available commands.\n\n"
          let gs'' = addItem gs' (Item "tool" 1 toolFunction)
          gameLoop gs''
    where mapWidth  = 55
          mapHeight = 25

startMessage :: Int -> IO ()
startMessage n
    | n == 1 = putStr "\nYou wake up to the chirping of birds.\n"
    | n == 2 = putStr "\nYou wake up to the wind rustling the leaves.\n"
	| n == 3 = putStr "\nYou wake up to the hot sun on your face.\n"
	| n == 4 = putStr "\nYou wake up to the fresh smell of flowers.\n"
    | n == 5 = putStr "\nYou wake up to the sudden fear of being alone.\n"

gameLoop :: GameState -> IO ()
gameLoop gs = do putStr "> "
                 hFlush stdout
                 command <- getLine
                 let (output, gs') = parseInput command gs
                 output
                 if checkDeath gs' then putStr "You died!!!\nGame Over" else gameLoop gs'

checkDeath :: GameState -> Bool
checkDeath (GameState _ hunger health _ _ map) = if hunger == 0 || health == 0
                                                 then True else False

-- MAP GENERATION ------------------------------------------------------
data Cell = Cell {cellType :: Char, visibility :: Bool}
makeCell :: String -> (Cell, String)
makeCell (x:xs) = (Cell x False, xs)

genEnvironment :: Int -> Int -> IO ()
genEnvironment w h = genEnvironment' 0 0 w h
genEnvironment' :: Int -> Int -> Int -> Int -> IO ()
genEnvironment' i j w h
    | i >= w && j >= (h-1)            = return ()
    | i >= w                          = genEnvironment' 0 (j+1) w h
    | otherwise                       = do x <- randomRIO (1,50)
    	                                   generateCell x
    	                                   genEnvironment' (i+1) j w h

generateCell :: Int -> IO ()
generateCell i
    | i == 1 = pushFile 't'
    | i == 2 = pushFile 'r'
    | i == 3 = pushFile 'p'
    | i > 26 = pushFile ','
    | otherwise = pushFile '.'

readEnvironment :: String -> Int -> Int -> [[Cell]]
readEnvironment s w h = readEnvironment' s [] 0 w h
readEnvironment' :: String -> [[Cell]] -> Int -> Int -> Int -> [[Cell]]
readEnvironment' s env i w h = if i < h then readEnvironment' s' env' (i+1) w h else env
    where env'    = env ++ [c]
          (c, s') = readEnvironment'' s [] 0 w
readEnvironment'' :: String -> [Cell] -> Int -> Int -> ([Cell], String)
readEnvironment'' s envRow i w = if i < w
                               then do let envRow' = envRow ++ [c]
                                       readEnvironment'' s' envRow' (i+1) w
                               else (envRow, s)
                               where (c, s') = makeCell s

pushFile :: Char -> IO ()
pushFile c = appendFile "tse.tmp" [c]

-- MAP VISUAL ----------------------------------------------------------
see :: GameState -> GameState
see (GameState (x, y) b c d e map) = (GameState (x, y) b c d e map')
    where map' = see' map 3 ((-3), (-3)) (x, y)
see' :: [[Cell]] -> Int -> (Int, Int) -> (Int, Int) -> [[Cell]]
see' map v (i, j) (x, y)
    | i == v && j == v = map
    | i >= (v+1) = see' map v (((-1)*v),(j+1)) (x,y)
    | (x+i) > (-1) && (y+j) > (-1) &&
      (x+i) < (length (map!!0)) && (y+j) < (length map) &&
      ((abs i) /= v || (abs j) /= v) = see' map' v ((i+1),j) (x,y)
    | otherwise = see' map v ((i+1),j) (x,y)
    where map' = replace2D map (x+i) (y+j) (Cell c True)
          (Cell c _) = (map!!(y+j))!!(x+i)

printMap :: GameState -> Int -> Int -> IO ()
printMap gs w h = printMap' gs (-1) (-1) w h >> putStr "\n"
printMap' :: GameState -> Int -> Int -> Int -> Int -> IO ()
printMap' gs@(GameState (x, y) _ _ _ _ map) i j w h
    | j >= h && i > w  = putStr "\n" >> return ()
    | j == y && i == x = putStr "@"  >> printMap' gs (i+1) j w h
    | i > w            = putStr "\n" >> printMap' gs (-1) (j+1) w h
    | j < 0 || j >= h  = putStr "-"  >> printMap' gs (i+1) j w h
    | i < 0 || i == w  = putStr "|"  >> printMap' gs (i+1) j w h
    | v == True        = putStr [c]  >> printMap' gs (i+1) j w h
    | otherwise        = putStr " "  >> printMap' gs (i+1) j w h
    where (Cell c v) = (map!!j)!!i

-- INVENTORY -----------------------------------------------------------
data Item = Item {
     name :: String,
     quantity :: Int,
     funct :: GameState -> (IO (), GameState)
     }

itemPlace :: [Item] -> String -> Int
itemPlace arr n = itemPlace' arr n 0
itemPlace' :: [Item] -> String -> Int -> Int
itemPlace' [] n i = (-1)
itemPlace' (x:xs) n i = if name == n then i else itemPlace' xs n (i+1)
    where (Item name _ _) = x

addItem :: GameState -> Item -> GameState
addItem (GameState a b c inv e f) it@(Item name quan fn)
    | place == (-1) = (GameState a b c (inv ++ [it]) e f)
    | otherwise = (GameState a b c (replace inv place (Item name (quan+quan') fn)) e f)
    where place = itemPlace inv name
          (Item _ quan' _) = inv!!(itemPlace inv name)

toolFunction :: GameState -> (IO (), GameState)
toolFunction gs@(GameState (x,y) hunger health inv turns map)
    | ct == 't' = do let gs' = GameState (x,y) hunger health inv turns (replace2D map x y (Cell ',' True))
                     (putStr "received 1 wood\n\n", addItem gs' (Item "wood" 1 nothingFunction))
    | ct == 'r' = do let gs' = GameState (x,y) hunger health inv turns (replace2D map x y (Cell '.' True))
                     (putStr "received 1 stone\n\n", addItem gs' (Item "stone" 1 nothingFunction))
    | otherwise = nothingFunction gs
    where (Cell ct _) = (map!!y)!!x

nothingFunction :: GameState -> (IO (), GameState)
nothingFunction gs = (putStr "nothing happens\n\n", gs)

-- Helper Functions for Array Replacement ------------------------------
replace2D :: [[a]] -> Int -> Int -> a -> [[a]]
replace2D (x:xs) r c newVal = if c == 0 then (replace x r newVal):xs
                              else x:(replace2D xs r (c-1) newVal)
replace :: [a] -> Int -> a -> [a]
replace (x:xs) n newVal = if n == 0 then newVal:xs
                          else x:(replace xs (n-1) newVal)

-- USER COMMANDS -------------------------------------------------------
parseInput :: String -> GameState -> (IO (), GameState)
parseInput command gs@(GameState (x, y) hunger health inv turns map)
    | c!!0 == "inv"                  = (putStr (invs gs ++ "\n"), gs)
    | c!!0 == "help"                 = do (putStr (help ++ "\n"), gs)
    | c!!0 == "status"               = (putStr (status gs ++ "\n\n"), gs)
    | c!!0 == "observe"              = (putStr (observe gs ++ "\n\n"), gs)
    | c!!0 == "north" || c!!0 == "w" = do let gs' = move gs 'n'
                                          (printMap gs' 55 25, gs')
    | c!!0 == "west"  || c!!0 == "a" = do let gs' = move gs 'w'
                                          (printMap gs' 55 25, gs')
    | c!!0 == "east"  || c!!0 == "d" = do let gs' = move gs 'e'
                                          (printMap gs' 55 25, gs')
    | c!!0 == "south" || c!!0 == "s" = do let gs' = move gs 's'
                                          (printMap gs' 55 25, gs')
    | c!!0 == "use"                  = use gs (c!!1)
    | c!!0 == "quit" || c!!0 == "exit" = quit gs
    | c!!0 == ""                     = (putStr "", gs)
    | otherwise                      = (putStr (command ++ ": command not found.\nType \"help\" to view available commands.\n\n"), gs)
    where c = splitOn " " command

invs :: GameState -> String
invs (GameState _ _ _ inv _ _) = invs' inv "Inventory:\n"
invs' :: [Item] -> String -> String
invs' [] list = list
invs' (x:xs) list = invs' xs (list ++ "| " ++ (show quan) ++ " " ++ name ++ "\n")
    where (Item name quan _) = x

help :: String
help = "Commands:\n"
     ++"|help - display this\n"
     ++"|inv - displays your inventory\n"
     ++"|use <item name> - use an item\n"
     ++"|status - shows your status\n"
     ++"|observe - observe your surroundings\n"
     ++"|w / North - move north\n"
     ++"|a / West - move west\n"
     ++"|d / East - move east\n"
     ++"|s / South - move south\n"
     ++"|quit / exit - close the game\n"

status :: GameState -> String
status (GameState _ hunger health _ _ _) = "Health: " ++ (show health) ++ "\nHunger: " ++ (show hunger)

observe :: GameState -> String
observe (GameState (x, y) _ _ _ _ map)
	| t == 't'  = "it's a tree"
    | t == 'r'  = "it's a lot of rocks"
    | t == 'p'  = "it's a herd of pigs"
    | otherwise = "there's not much around"
    where (Cell t _) = (map!!y)!!x

move :: GameState -> Char -> GameState
move gs@(GameState (x, y) h c d t f) dir
    | dir == 'n' && y > 0  = do let gs' = GameState (x,(y-1)) (h-1) c d (t+1) f
                                see gs'
    | dir == 'w' && x > 0  = do let gs' = GameState ((x-1),y) (h-1) c d (t+1) f
                                see gs'
    | dir == 'e' && x < 54 = do let gs' = GameState ((x+1),y) (h-1) c d (t+1) f
                                see gs'
    | dir == 's' && y < 24 = do let gs' = GameState (x,(y+1)) (h-1) c d (t+1) f
                                see gs'
    | otherwise = gs

use :: GameState -> String -> (IO (), GameState)
use gs@(GameState _ _ _ inv _ _) itname
    | itemPlace inv itname == -1 = (putStr "you don't have that item", gs)
    | otherwise = fn gs
    where (Item _ _ fn) = inv!!(itemPlace inv itname)

quit :: GameState -> (IO (), GameState)
quit gs = (exitSuccess, gs)
