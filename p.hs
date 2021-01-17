import Data.List
import Data.Ord
import Split
import System.Directory
import System.IO
import Text.Printf
import qualified Data.Set as Set (toList, fromList)

data Game = Game [[(Char)]] deriving (Show)

--newline
changeLine :: IO ()
changeLine = putStr "\n"

--removes duplicates in a list
mkUniq :: Ord a => [a] -> [a]
mkUniq = Set.toList . Set.fromList

--constructs a Game board
constructGame :: [String] -> Game
constructGame a = (Game a)

--Prints the board
printGame :: [String] -> IO ()
printGame [] = return ()
printGame (x:xs) = do putStrLn x
                      printGame xs

--check if board has exactly 1 start point and 1 end point
hasStart :: String -> Bool
hasStart a = do let startc = length $ filter (=='6') a
                let startca = length $ filter (=='8') a
                let startcb = length $ filter (=='9') a
                let startcc = length $ filter (=='a') a
                (startc+startca+startcb+startcc) == 1

hasEnd :: String -> Bool
hasEnd a = do let endcount = length $ filter (== '7') a
              endcount == 1

hasStartEnd :: String -> Bool
hasStartEnd a = hasStart a && hasEnd a

--locate the start of the map
locateStart :: String -> Int -> Int
locateStart a pos = case a of
  [] -> pos
  (x:xs) -> case x of
            '6' -> pos
            '8' -> pos
            '9' -> pos
            'a' -> pos
            _   -> locateStart xs pos+1

--update position (startx,starty) on Board a as 1
updateBoard :: [String] -> Int -> Int -> [String]
updateBoard a startx starty = take startx a ++ [take starty (a!!startx) ++ "1" ++ drop (starty+1) (a!!startx)] ++ drop (startx+1) a
--find an end on a board
findEnd :: [String] -> Int -> Int -> Bool
findEnd a startx starty = do let maxy = (length (a!!0)) -1
                             let maxx = (length a) -1
                             if (startx > maxx || startx < 0 || starty > maxy || starty < 0) then False
                             else if ((a!!startx!!starty)=='7') then True
                             else if ((a!!startx!!starty)=='1') then False
                             else 
                               do let b = updateBoard a startx starty
                                  (findEnd b (startx-1) starty) || (findEnd b (startx+1) starty) || (findEnd b startx (starty-1)) || (findEnd b startx (starty+1))
--see if there is an unblocked route from start to end
existRoute :: [String] -> Bool
existRoute a = do let start = locateStart (concat a) 0
                  let rowlength = length (a!!0)
                  let exist = findEnd a (start`div`rowlength) (start`mod`rowlength)
                  exist

--check if there is syntax error and numerical value error in a single move
moveNoError :: String -> Bool
moveNoError move = do if (move == "Left" || move == "Right" || move == "Up" || move == "Down") then True
                      else if (take 4 move == "Loop") then do let loops = remain (drop 4 move) "0123456789"
                                                              if (loops =="0"||loops =="1"||loops=="2"||loops=="3"||loops=="4"||loops=="5") then
                                                                do let movesInLooper = drop 4 move
                                                                   let movesInLoope = remove movesInLooper "0123456789{}"
                                                                   let movesInLoop = splitOn "," movesInLoope
                                                                   if (functionNoError movesInLoop) then True
                                                                   else False
                                                                else False
                      else if (take 4 move == "Cond") then do let color = remain (drop 4 move) "0123456789"
                                                              if (color == "2" || color =="3"||color=="4") then
                                                                do let moveInCondd = drop 4 move
                                                                   let mInC = remove moveInCondd "0123456789{}"
                                                                   if (mInC=="Right"||mInC=="Left"||mInC=="Up"||mInC=="Down") then True
                                                                   else False
                                                              else False
                      else False

--check if there is syntax error in function
functionNoError :: [String] -> Bool
functionNoError function = case function of
  [] -> True
  (x:xs) -> moveNoError x && functionNoError xs

--remain selected items from string
remain :: String -> String -> String
remain xs toRemain = [ x | x <- xs, (x `elem` toRemain) ]

--remove selected items from string
remove :: String -> String -> String
remove xs toRemove = [ x | x <- xs, not (x `elem` toRemove) ]

--decompose loops in moves
decomposeLoops :: [String] -> [String]
decomposeLoops moves = case moves of
  [] -> []
  (x:xs) -> case (take 4 x) of
            "Loop" -> do let moveInLooper = drop 4 x
                         let moveInLoope = remove moveInLooper "01234567890{}"
                         let moveInLoop = splitOn "," moveInLoope
                         let times = (remain x "012345")
                         if (times == "0") then decomposeLoops xs
                         else if (times == "1") then moveInLoop ++ decomposeLoops xs
                         else if (times == "2") then moveInLoop ++ moveInLoop ++ decomposeLoops xs
                         else if (times == "3") then moveInLoop ++ moveInLoop ++ moveInLoop ++ decomposeLoops xs
                         else if (times == "4") then moveInLoop ++ moveInLoop ++ moveInLoop ++ moveInLoop ++ decomposeLoops xs
                         else if (times == "5") then moveInLoop ++ moveInLoop ++ moveInLoop ++ moveInLoop ++ moveInLoop ++ decomposeLoops xs
                         else decomposeLoops xs
            _      -> [x] ++ decomposeLoops xs

--search the obstacles of a point in 4 directions
--the third returning element is strictly used for testing CondPossible only
searchUpBorder :: [String] -> Int -> Int -> Int -> String -> ([String],Int,String)
searchUpBorder a x y stars color = do let lastTile = (a!!(x+1)!!y)
                                      if (x<0) then (a,stars,"6")
                                      else if ((a!!x!!y)=='1') then (a,stars,"6")
                                      else if ((a!!x!!y)=='2') then
                                        do let ab = updateBoardn a x y "8"
                                           let b = updateLastTile ab (x+1) y lastTile
                                           if (color=="2") then (b,stars,"2")
                                           else searchUpBorder b (x-1) y stars color
                                      else if ((a!!x!!y)=='3') then
                                        do let ab = updateBoardn a x y "9"
                                           let b = updateLastTile ab (x+1) y lastTile
                                           if (color=="3") then (b,stars,"3")
                                           else searchUpBorder b (x-1) y stars color
                                      else if ((a!!x!!y)=='4') then
                                        do let ab = updateBoardn a x y "a"
                                           let b = updateLastTile ab (x+1) y lastTile
                                           if (color=="4") then (b,stars,"4")
                                           else searchUpBorder b (x-1) y stars color
                                      else if ((a!!x!!y)=='5') then
                                        do let ab = updateBoardn a x y "6"
                                           let b = updateLastTile ab (x+1) y lastTile
                                           searchUpBorder b (x-1) y (stars+1) color
                                      else 
                                        do let ab = updateBoardn a x y "6"
                                           let b = updateLastTile ab (x+1) y lastTile
                                           searchUpBorder b (x-1) y stars color

searchDownBorder :: [String] -> Int -> Int -> Int -> String -> ([String],Int,String)
searchDownBorder a x y stars color = do let maxx = (length a) - 1
                                        let lastTile = (a!!(x-1)!!y)
                                        if (x>maxx) then (a,stars,"6")
                                        else if ((a!!x!!y)=='1') then (a,stars,"6")
                                        else if ((a!!x!!y)=='2') then
                                          do let ab = updateBoardn a x y "8"
                                             let b = updateLastTile ab (x-1) y lastTile
                                             if (color=="2") then (b,stars,"2")
                                             else searchDownBorder b (x+1) y stars color
                                        else if ((a!!x!!y)=='3') then
                                          do let ab = updateBoardn a x y "9"
                                             let b = updateLastTile ab (x-1) y lastTile
                                             if (color=="3") then (b,stars,"3")
                                             else searchDownBorder b (x+1) y stars color
                                        else if ((a!!x!!y)=='4') then
                                          do let ab = updateBoardn a x y "a"
                                             let b = updateLastTile ab (x-1) y lastTile
                                             if (color=="4") then (b,stars,"4")
                                             else searchDownBorder b (x+1) y stars color
                                        else if ((a!!x!!y)=='5') then
                                          do let ab = updateBoardn a x y "6"
                                             let b = updateLastTile ab (x-1) y lastTile
                                             searchDownBorder b (x+1) y (stars+1) color
                                        else 
                                          do let ab = updateBoardn a x y "6"
                                             let b = updateLastTile ab (x-1) y lastTile
                                             searchDownBorder b (x+1) y stars color

searchLeftBorder :: [String] -> Int -> Int -> Int -> String -> ([String],Int,String)
searchLeftBorder a x y stars color = do let lastTile = (a!!x!!(y+1))
                                        if (y<0) then (a,stars,"6")
                                        else if ((a!!x!!y)=='1') then (a,stars,"6")
                                        else if ((a!!x!!y)=='2') then
                                          do let ab = updateBoardn a x y "8"
                                             let b = updateLastTile ab x (y+1) lastTile
                                             if (color=="2") then (b,stars,"2")
                                             else searchLeftBorder b x (y-1) stars color
                                        else if ((a!!x!!y)=='3') then
                                          do let ab = updateBoardn a x y "9"
                                             let b = updateLastTile ab x (y+1) lastTile
                                             if (color=="3") then (b,stars,"3")
                                             else searchLeftBorder b x (y-1) stars color
                                        else if ((a!!x!!y)=='4') then
                                          do let ab = updateBoardn a x y "a"
                                             let b = updateLastTile ab x (y+1) lastTile
                                             if (color=="4") then (b,stars,"4")
                                             else searchLeftBorder b x (y-1) stars color
                                        else if ((a!!x!!y)=='5') then
                                          do let ab = updateBoardn a x y "6"
                                             let b = updateLastTile ab x (y+1) lastTile
                                             searchLeftBorder b x (y-1) (stars+1) color
                                        else 
                                          do let ab = updateBoardn a x y "6"
                                             let b = updateLastTile ab x (y+1) lastTile
                                             searchLeftBorder b x (y-1) stars color
            
searchRightBorder :: [String] -> Int -> Int -> Int -> String -> ([String],Int,String)
searchRightBorder a x y stars color = do let maxy = (length (a!!0)) - 1
                                         let lastTile = (a!!x!!(y-1))
                                         if (y>maxy) then (a,stars,"6")
                                         else if ((a!!x!!y)=='1') then (a,stars,"6")
                                         else if ((a!!x!!y)=='2') then
                                           do let ab = updateBoardn a x y "8"
                                              let b = updateLastTile ab x (y-1) lastTile
                                              if (color=="2") then (b,stars,"2")
                                              else searchRightBorder b x (y+1) stars color
                                         else if ((a!!x!!y)=='3') then
                                           do let ab = updateBoardn a x y "9"
                                              let b = updateLastTile ab x (y-1) lastTile
                                              if (color=="3") then (b,stars,"3")
                                              else searchRightBorder b x (y+1) stars color
                                         else if ((a!!x!!y)=='4') then
                                           do let ab = updateBoardn a x y "a"
                                              let b = updateLastTile ab x (y-1) lastTile
                                              if (color=="4") then (b,stars,"4")
                                              else searchRightBorder b x (y+1) stars color 
                                         else if ((a!!x!!y)=='5') then
                                           do let ab = updateBoardn a x y "6"
                                              let b = updateLastTile ab x (y-1) lastTile
                                              searchRightBorder b x (y+1) (stars+1) color
                                         else                                           
                                           do let ab = updateBoardn a x y "6"
                                              let b = updateLastTile ab x (y-1) lastTile
                                              searchRightBorder b x (y+1) stars color
--get column elements of a board
getColumn :: [String] -> Int -> String
getColumn a starty = transpose a !! starty

--check if a condtional move can be executed successfully (on the same row/column and no blockades)
condPossible :: [String] -> Int -> Int -> String -> String -> Bool
condPossible a startx starty color currentmove = do if (currentmove=="Up") then
                                                      do let (_,_,t) = searchUpBorder a (startx-1) starty 0 color 
                                                         color == t
                                                    else if (currentmove=="Down") then
                                                      do let (_,_,t) = searchDownBorder a (startx+1) starty 0 color 
                                                         color == t
                                                    else if (currentmove=="Left") then
                                                      do let (_,_,t) = searchLeftBorder a startx (starty-1) 0 color 
                                                         color == t
                                                    else if (currentmove=="Right") then
                                                      do let (_,_,t) = searchRightBorder a startx (starty+1) 0 color 
                                                         color == t
                                                    else False

--update position (row, column) on Board a as a certain [char]
updateBoardn :: [String] -> Int -> Int -> String -> [String]
updateBoardn a startx starty value = take startx a ++ [take starty (a!!startx) ++ value ++ drop (starty+1) (a!!startx)] ++ drop (startx+1) a

--update lastTile
updateLastTile :: [String] -> Int -> Int -> Char -> [String]
updateLastTile a startx starty value = case value of
                                       '8' -> updateBoardn a startx starty "2"
                                       '9' -> updateBoardn a startx starty "3"
                                       'a' -> updateBoardn a startx starty "4"
                                       _   -> updateBoardn a startx starty "0"
               
--execute moves on board
executeMoves :: [String] -> [String] -> [String] -> Int -> IO ()
executeMoves a function moves stars = do let start = locateStart (concat a) 0
                                         let rowlength = length (a!!0)
                                         let startx = start`div`rowlength
                                         let starty = start`mod`rowlength
                                         changeLine
                                         if (moves == []) then
                                            do playGame a function [] stars
                                         else 
                                            do let currentmove = head moves
                                               if (currentmove == "Up") then
                                                  do if (length moves > 1 && take 4 (moves!!1) == "Cond") then
                                                       do let cond = moves!!1
                                                          let color = remain cond "1234567890"
                                                          let condMove = remove (drop 4 cond) "0123456789{}"
                                                          if (condPossible a startx starty color currentmove) then
                                                            do let (b,nstars,_) = searchUpBorder a (startx-1) starty stars color
                                                               printGame b
                                                               if (hasEnd $ concat b) then executeMoves b function ([condMove]++drop 2 moves) nstars
                                                               else
                                                                 do putStrLn . printf"Success! Also got %i stars!" $ nstars
                                                                    game (Game b)
                                                          else do putStrLn "Conditional move not possible!"
                                                                  playGame a function [] stars
                                                     else 
                                                       do let (b,nstars,_) = searchUpBorder a (startx-1) starty stars "0"
                                                          printGame b
                                                          if (hasEnd $ concat b) then
                                                            do executeMoves b function (tail moves) nstars
                                                          else
                                                            do putStrLn . printf "Success! Also got %i stars!" $ nstars
                                                               game (Game b)
                                               else if (currentmove == "Down") then
                                                  do if (length moves > 1 && take 4 (moves!!1) == "Cond") then
                                                       do let cond = moves!!1
                                                          let color = remain cond "1234567890"
                                                          let condMove = remove (drop 4 cond) "0123456789{}"
                                                          if (condPossible a startx starty color currentmove) then
                                                            do let (b,nstars,_) = searchDownBorder a (startx+1) starty stars color
                                                               printGame b
                                                               if (hasEnd $ concat b) then executeMoves b function ([condMove] ++ drop 2 moves) nstars
                                                               else
                                                                 do putStrLn . printf "Success! Also got %i stars!" $ nstars
                                                                    game (Game b)
                                                          else do putStrLn "Conditional move not possibe!"
                                                                  playGame a function [] stars
                                                     else
                                                       do let (b,nstars,_) = searchDownBorder a (startx+1) starty stars "0"
                                                          printGame b
                                                          if (hasEnd $ concat b) then
                                                            do executeMoves b function (tail moves) nstars
                                                          else
                                                            do putStrLn . printf "Success! Also got %i stars!" $ nstars
                                                               game (Game b)
                                               else if (currentmove == "Left") then
                                                  do if (length moves > 1 && take 4 (moves!!1) == "Cond") then
                                                       do let cond = moves!!1
                                                          let color = remain cond "1234567890"
                                                          let condMove = remove (drop 4 cond) "0123456789{}"
                                                          if (condPossible a startx starty color currentmove) then
                                                            do let (b,nstars,_) = searchLeftBorder a startx (starty-1) stars color
                                                               printGame b
                                                               if (hasEnd $ concat b) then executeMoves b function ([condMove]++ drop 2 moves) nstars
                                                               else
                                                                 do putStrLn . printf "Success! Also got %i stars!" $ nstars
                                                                    game (Game b)
                                                          else do putStrLn "Conditional move not possible"
                                                                  playGame a function [] stars
                                                     else 
                                                       do let (b,nstars,_) = searchLeftBorder a startx (starty-1) stars "0"
                                                          printGame b
                                                          if (hasEnd $ concat b) then
                                                            do executeMoves b function (tail moves) nstars
                                                          else
                                                            do putStrLn . printf "Success! Also got %i stars!" $ nstars
                                                               game (Game b)
                                               else if (currentmove == "Right") then
                                                  do if (length moves > 1 && take 4 (moves!!1) == "Cond") then
                                                       do let cond = moves!!1
                                                          let color = remain cond "1234567890"
                                                          let condMove = remove (drop 4 cond) "0123456789{}"
                                                          if (condPossible a startx starty color currentmove) then
                                                            do let (b,nstars,_) = searchRightBorder a startx (starty+1) stars color
                                                               printGame b
                                                               if (hasEnd $ concat b) then executeMoves b function ([condMove]++drop 2 moves) nstars
                                                               else
                                                                 do putStrLn . printf "Success! Also got %i stars!" $ nstars
                                                                    game (Game b)
                                                          else do putStrLn "Conditional move not possible"
                                                                  playGame a function [] stars
                                                     else 
                                                       do let (b,nstars,_) = searchRightBorder a startx (starty+1) stars "0"
                                                          printGame b
                                                          if (hasEnd $ concat b) then
                                                            do executeMoves b function (tail moves) nstars
                                                          else
                                                            do putStrLn . printf "Success! Also got %i stars!" $ nstars
                                                               game (Game b)
                                               else do putStrLn"Error in executeMoves function"
                                                       playGame a function [] stars
                              
--the interactive play function
playGame :: [String] -> [String] -> [String] -> Int -> IO ()
playGame a function moves stars = do putStr"Enter the next direction: "
                                     hFlush stdout
                                     moveinput <- getLine
                                     let args = words moveinput
                                     if (length args > 1) then
                                        do putStrLn "too many directions typed! Only type 1 direction"
                                           playGame a function moves stars
                                     else if (length args == 0) then executeMoves a function moves stars
                                     else if (args!!0 == "Function") then playGame a function (moves++function) stars
                                     else if (args!!0 == "quit") then game (Game a)
                                     else if (args!!0 == "printf") then 
                                       do print function
                                          playGame a function moves stars
                                     else if (args!!0 == "print") then 
                                       do print moves
                                          playGame a function moves stars
                                     else if (args!!0 == "show") then 
                                       do printGame a
                                          playGame a function moves stars
                                     else if (args!!0 == "undo") then
                                       do if (length moves == 0) then
                                            do putStrLn "empty list"
                                               playGame a function moves stars
                                          else playGame a function (reverse $ tail $ reverse moves) stars
                                     else if (args!!0 == "hint") then
                                       do let solutions = getSolutions a
                                          if (solutions == []) then 
                                             do putStrLn "This map is not solvable! So no hints!"
                                                playGame a function moves stars
                                          else
                                             do let hint = head $ shortest solutions
                                                putStrLn ("maybe try " ++ hint ++ "?")
                                                playGame a function moves stars
                                     else
                                        do if (moveNoError (args!!0)) then
                                              do let decomposedm = decomposeLoops args
                                                 playGame a function (moves++decomposedm) stars
                                           else
                                              do putStrLn "error in moved typed"
                                                 playGame a function moves stars

--when checking for solutions, see if the current direction can be applied once more
nextmovePossible :: [String] -> Int -> Int -> Bool
nextmovePossible a startx starty = do let maxy = (length (a!!0)) -1
                                      let maxx = (length a) -1
                                      if (startx > maxx || startx < 0 || starty > maxy || starty < 0) then False
                                      else if ((a!!startx!!starty)=='1') then False
                                      else True

--solve the board, inputs board returns a list of instructions
solveGame :: [String] -> Int -> Int -> [String] -> String -> [String]
solveGame a startx starty sol lastmove = do let maxy = (length (a!!0)) -1
                                            let maxx = (length a) -1
                                            if (startx > maxx || startx < 0 || starty > maxy || starty < 0) then []
                                            else if ((a!!startx!!starty)=='7') then sol++["End"]
                                            else if ((a!!startx!!starty)=='1') then []
                                            else if ((a!!startx!!starty)=='2') then
                                              do let b = updateBoard a startx starty
                                                 solveGame b (startx-1) starty (sol++["2Up"]) "Up" ++solveGame b (startx+1) starty (sol++["2Down"]) "Down" ++ solveGame b startx (starty-1) (sol++["2Left"]) "Left" ++solveGame b startx (starty+1) (sol++["2Right"]) "Right"
                                            else if ((a!!startx!!starty)=='3') then
                                              do let b = updateBoard a startx starty
                                                 solveGame b (startx-1) starty (sol++["3Up"]) "Up" ++solveGame b (startx+1) starty (sol++["3Down"]) "Down" ++ solveGame b startx (starty-1) (sol++["3Left"]) "Left" ++solveGame b startx (starty+1) (sol++["3Right"]) "Right"
                                            else if ((a!!startx!!starty)=='4') then
                                              do let b = updateBoard a startx starty
                                                 solveGame b (startx-1) starty (sol++["4Up"]) "Up" ++solveGame b (startx+1) starty (sol++["4Down"]) "Down" ++ solveGame b startx (starty-1) (sol++["4Left"]) "Left" ++solveGame b startx (starty+1) (sol++["4Right"]) "Right"
                                            else 
                                              do let b = updateBoard a startx starty
                                                 if (lastmove=="Up") then 
                                                    do if (nextmovePossible a (startx-1) starty) then solveGame b (startx-1) starty (sol++["Up"]) lastmove
                                                       else solveGame b startx (starty-1) (sol++["Left"]) "Left" ++solveGame b startx (starty+1) (sol++["Right"]) "Right"
                                                 else if (lastmove=="Down") then 
                                                    do if (nextmovePossible a (startx+1) starty) then solveGame b (startx+1) starty (sol++["Down"]) lastmove
                                                       else solveGame b startx (starty-1) (sol++["Left"]) "Left" ++solveGame b startx (starty+1) (sol++["Right"]) "Right"
                                                 else if (lastmove=="Left") then 
                                                    do if (nextmovePossible a startx (starty-1)) then solveGame b startx (starty-1) (sol++["Left"]) lastmove
                                                       else solveGame b (startx-1) starty (sol++["Up"]) "Up" ++solveGame b (startx+1) starty (sol++["Down"]) "Down"
                                                 else if (lastmove=="Right") then
                                                    do if (nextmovePossible a startx (starty+1)) then solveGame b startx (starty+1) (sol++["Right"]) lastmove
                                                       else solveGame b (startx-1) starty (sol++["Up"]) "Up" ++solveGame b (startx+1) starty (sol++["Down"]) "Down"
                                                 else []

--print 1 solution on 1 line
printSol :: [[String]] -> IO ()
printSol a = case a of
  [] -> return ()
  (x:xs) -> do print x
               printSol xs

--joins the same adjacent steps in each solution
joinOne :: [String] -> [String]
joinOne a = case a of
  [] -> []
  (x:[]) -> [x]
  (x:xx:xs) -> if (x==xx) then joinOne ([xx]++xs)
               else if ((tail x)==xx || x == (tail xx)) then joinOne ([x]++xs)
               else [x] ++ joinOne ([xx]++xs)

joinSol :: [[String]] -> [[String]]
joinSol a = case a of
  [] -> []
  (x:xs) -> [joinOne x] ++ joinSol xs

--gets the list of possible solutions
getSolutions :: [String] -> [[String]]
getSolutions a = do let start = locateStart (concat a) 0
                    let rowlength = length (a!!0)
                    let startx = start`div`rowlength
                    let starty = start`mod`rowlength
                    let solutions = solveGame a startx starty [] "Up" ++ solveGame a startx starty [] "Down" ++ solveGame a startx starty [] "Left" ++ solveGame a startx starty [] "Right"
                    let seperatedss = splitWhen (=="End") solutions
                    let seperateds = tail (reverse seperatedss) -- removes the empty list on the end of solutions
                    mkUniq (joinSol seperateds)
   
--gets solution with minimum value of moves
shortest :: [[a]] -> [a]
shortest = minimumBy (comparing length)

--check if the map is a rectangle
checkRectangleMap :: [String] -> Bool
checkRectangleMap a = (length $ minimumBy (comparing length) a)== (length $ maximumBy (comparing length) a)

--check if the board contents is valid
boardContentsOk :: String -> Bool
boardContentsOk a = a == (remain a "0123456789a")

--shrink one output to loops
shrink :: [String] -> [String]
shrink a = case a of 
  [] -> []
  (x:[]) -> [x]
  (x:xx:[])-> [x]++[xx]
  (x:xx:y:[])-> if ((head x) == 'Z' && (remove x "Z") == (xx++","++y)) then ["Z"++x]
                else [x]++[xx]++[y]
  (x:xx:y:yy:xs) -> if (([x]++[xx]) == ([y]++[yy])) then shrink (["Z"++x++","++xx]++xs)
                    else if ((head x) == 'Z' && (remove x "Z") == (xx++","++y)) then shrink (["Z"++x]++([yy]++xs))
                    else [x] ++ shrink ([xx]++[y]++[yy]++xs)

--find function occurrence 
findTripleOccurence :: [String] -> [String] -> Int -> Int
findTripleOccurence _ [] times = times
findTripleOccurence _ (_:[]) times = times
findTripleOccurence _ (_:_:[]) times = times
findTripleOccurence t (x:xx:y:xs) times = if (t== ([x]++[xx]++[y])) then findTripleOccurence t xs times+1
                                          else findTripleOccurence t ([xx]++[y]++xs) times

--find all triple occurences of every triple in a list
listOfTriple :: [String] -> [Int]
listOfTriple a = case a of
  [] -> [0]
  (_:[]) -> [0]
  (_:_:[]) -> [0]
  (x:xx:y:xs) -> [findTripleOccurence ([x]++[xx]++[y]) xs 1] ++ listOfTriple ([xx]++[y]++xs)

--get index of maximum value of a list
maxi :: [Int] -> (Int,Int)
maxi a = maximumBy (comparing fst) (zip a [0..])

--shrink one output to function
fshrink :: [String] -> [String] -> [String]
fshrink _ [] = []
fshrink _ (x:[]) = [x]
fshrink _ (x:xx:[]) = [x]++[xx]
fshrink func (x:xx:y:xs) = if (func == ([x]++[xx]++[y])) then ["Function"]++ fshrink func xs
                           else [x] ++ fshrink func ([xx]++[y]++xs)

--main game logic
game :: Game -> IO ()
game (Game a) = do
                                    hSetBuffering stdin NoBuffering
                                    changeLine
                                    putStrLn "Select one of the following options:"
                                    putStrLn "1. load <<filename>>"
                                    putStrLn "2. check"
                                    putStrLn "3. solve"
                                    putStrLn "4. quit"
                                    putStrLn "5. play"
                                    putStrLn "6. show (current board)"
                                    putStr "Enter your choice: "
                                    hFlush stdout
                                    optionLine <- getLine
                                    changeLine
                                    let args = words optionLine
                                    if ((head args) == "load") then
                                       if (length args < 2) then 
                                         do putStrLn "No file typed!"
                                            game (Game a)
                                       else
                                         do fileChecker <- doesFileExist (args!!1)
                                            if (fileChecker) then
                                              do fileContents <- readFile (args!!1)
                                                 let dat = lines fileContents
                                                 if (checkRectangleMap dat && boardContentsOk (concat dat)) then
                                                   do let board = constructGame dat
                                                      putStrLn "Read board successfully!"
                                                      putStrLn "Initial board:"
                                                      printGame dat
                                                      game board
                                                 else
                                                   do putStrLn "Error in board shape or board contents. No file loaded"
                                                      game (Game a)
                                            else
                                              do putStrLn "Invalid input! The file does not exist in this directory"
                                                 game (Game a)
                                    else if ((head args) == "solve" || (head args) == "check") then
                                       do if (hasStartEnd (concat a)) then
                                            do if (existRoute a) then
                                                 do let joineds = getSolutions a
                                                    if (length joineds == 0) then
                                                      do putStrLn "No solutions found!"
                                                         game (Game a)
                                                    else if (length joineds /= 0 && (head args) == "check" ) then
                                                      do putStrLn "The map is solvable"
                                                         game (Game a)
                                                    else
                                                      do printSol joineds
                                                         putStrLn . printf "Total %i possible solutions" $ length joineds
                                                         putStr "The shortest solution is: "
                                                         print $ shortest joineds
                                                         let shrinkedSol = map shrink joineds
                                                         changeLine
                                                         putStrLn "printing shrinked solutions"
                                                         printSol shrinkedSol
                                                         putStrLn "The shortest shrinked solution is: "
                                                         let shortSol = shortest shrinkedSol
                                                         print $ shortSol
                                                         let fcount = filter (>1) (listOfTriple shortSol)
                                                         if (fcount == []) then game (Game a)
                                                         else
                                                           do let findex = snd $ maxi (listOfTriple shortSol)
                                                              let func = [shortSol!!findex]++[shortSol!!(findex+1)]++[shortSol!!(findex+2)]
                                                              let fSol = fshrink func shortSol
                                                              changeLine
                                                              putStrLn "printing functioned form of shortest shrinked solution"
                                                              print fSol
                                                              putStr "  with Function: "
                                                              print func
                                                              game (Game a)
                                               else 
                                                 do putStrLn "No unblocked paths from start to end found, not solvable"
                                                    game (Game a)
                                          else 
                                            do putStrLn "Error with Start point or End point, not solvable"
                                               game (Game a)
                                    else if ((head args) == "quit") then putStrLn "Goodbye!"
                                    else if ((head args) == "play") then
                                       do if (hasStartEnd $ concat a) then
                                            do putStrLn"Type \"quit\" to return to main menu"
                                               putStrLn"Type \"print\" to show entered moves"
                                               putStrLn"Type \"printf\" to show entered function"
                                               putStrLn"Type \"undo\" to undo"
                                               putStrLn"Type \"hint\" for a hint"
                                               if (length args > 1) then
                                                 do let function = tail args 
                                                    if (functionNoError function) then
                                                      do let decomposedf = decomposeLoops function
                                                         playGame a decomposedf [] 0
                                                    else
                                                      do putStrLn "Error in the function"
                                                         game (Game a)                    
                                               else playGame a [] [] 0
                                          else 
                                             do putStrLn "Error with start point or End point! cannot start game!"
                                                game (Game a)                 
                                    else if ((head args) == "show") then
                                       do printGame a
                                          game (Game a)
                                    else
                                       do putStrLn "Please enter a valid option!"
                                          game (Game a)

main :: IO ()
main = game (Game ([]))
