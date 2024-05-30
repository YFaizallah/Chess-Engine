import Data.Char (ord)

type Location = (Char, Int)
data Player = White | Black deriving (Show, Eq)
data Piece = P Location | N Location | K Location | Q Location | R Location| B Location deriving (Show, Eq)
type Board = (Player, [Piece], [Piece])

--- a)
setBoard :: Board
setBoard = (White, [R ('h', 1), N ('g', 1), B ('f', 1), K ('e', 1), Q ('d', 1), B ('c', 1), N ('b', 1), R ('a', 1), P ('h', 2), P ('g', 2), P ('f', 2), P ('e', 2), P ('d', 2), P ('c', 2), P ('b', 2), P ('a', 2)], [R ('h', 8), N ('g', 8), B ('f', 8), K ('e', 8), Q ('d', 8), B ('c', 8), N ('b', 8), R ('a', 8),P ('h', 7), P ('g', 7), P ('f', 7), P ('e', 7), P ('d', 7), P ('c', 7), P ('b', 7), P ('a', 7)])
--- b)  
visualizeBoard :: Board -> String
visualizeBoard (player, whitePieces, blackPieces) = headerRow ++ " \n " ++  board ++ " \n " ++ " Turn: " ++ show player
  
  where
    headerRow = "   a    b    c    d    e    f    g    h"
    board = printBoard (reverse [1..8])
    printBoard :: [Int] -> String
    printBoard [] = ""
    printBoard (rowNum:rowNums) = intToString rowNum ++ " |" ++ printRowPieces rowNum ['a'..'h'] ++ printBoard rowNums
    printRowPieces :: Int -> [Char] -> String
    printRowPieces _ [] = " \n "
    printRowPieces rowNum (col:cols) = getPiece (col, rowNum) ++ " |" ++ printRowPieces rowNum cols
    getPiece :: Location -> String
    getPiece loc = case findLoc loc (whitePieces ++ blackPieces) of
      Just (P _) -> if getLocPieceW whitePieces loc then "PW" else if getLocPieceB blackPieces loc then "PB" else ""
      Just (N _) -> if getLocPieceW whitePieces loc then "NW" else if getLocPieceB blackPieces loc then "NB" else ""
      Just (B _) -> if getLocPieceW whitePieces loc then "BW" else if getLocPieceB blackPieces loc then"BB" else ""
      Just (R _) -> if getLocPieceW whitePieces loc then "RW" else if getLocPieceB blackPieces loc then "RB" else ""
      Just (Q _) -> if getLocPieceW whitePieces loc then "QW" else if getLocPieceB blackPieces loc then "QB" else ""
      Just (K _) -> if getLocPieceW whitePieces loc then "KW" else if getLocPieceB blackPieces loc then "KB" else ""
      Nothing    -> "  "  

intToString :: Int -> String
intToString n = show n

getLocPieceW :: [Piece] -> Location ->Bool
getLocPieceW [] _ = False
getLocPieceW (h:t) (col,row) = let (col1,row1) = getPieceLocation h in if col == col1 && row == row1 then True else getLocPieceW t (col,row)

getLocPieceB :: [Piece] -> Location -> Bool
getLocPieceB [] _ = False
getLocPieceB (h:t) (col,row) = let (col1,row1) = getPieceLocation h in if col == col1 && row == row1 then True else getLocPieceB t (col,row)

findLoc :: Location -> [Piece] -> Maybe Piece
findLoc _ [] = Nothing
findLoc loc (piece:pieces)
    | getPieceLocation piece == loc = Just piece
    | otherwise = findLoc loc pieces


getPieceLocation :: Piece -> Location
getPieceLocation (P loc) = loc
getPieceLocation (N loc) = loc
getPieceLocation (B loc) = loc
getPieceLocation (R loc) = loc
getPieceLocation (Q loc) = loc
getPieceLocation (K loc) = loc

--- c)
isLegal :: Piece -> Board -> Location -> Bool
isLegal (P loc) board dest = legalPawnMove board (P loc) dest
isLegal (R loc) board dest = legalRookMove board (R loc) dest
isLegal (N loc) board dest = legalKnightMove board (N loc) dest
isLegal (B loc) board dest = legalBishopMove (B loc) dest board 
isLegal (Q loc) board dest = legalQueenMove board (Q loc) dest 
isLegal (K loc) board dest = legalKingMove board (K loc) dest

legalPawnMove :: Board -> Piece -> Location -> Bool
legalPawnMove board piece destLoc
  | player == White && row == 2 = destCol == col && (destRow == row + 1 || destRow == row + 2) && getLocPieceW1 wplayer (col,row + 1) && getLocPieceB1 bplayer (col,row + 1) && getLocPieceW1 wplayer destLoc && getLocPieceB1 bplayer destLoc 
  | player == Black && row == 7 = destCol == col && (destRow == row - 1 || destRow == row - 2) && getLocPieceW1 wplayer (col,row - 1) && getLocPieceB1 bplayer (col,row - 1) && getLocPieceW1 wplayer destLoc && getLocPieceB1 bplayer destLoc
  | player == White && (destCol == succ col || destCol == pred col) = destCol == col && destRow == row + 1 && getLocPieceW1 wplayer destLoc && getLocPieceB bplayer destLoc
  | player == Black && (destCol == succ col || destCol == pred col) = destCol == col && destRow == row - 1 && getLocPieceB1 bplayer destLoc && getLocPieceW wplayer destLoc
  | player == White = destCol == col && destRow == row + 1 && getLocPieceW1 wplayer destLoc && getLocPieceB bplayer destLoc
  | player == Black = destCol == col && destRow == row - 1 && getLocPieceB1 bplayer destLoc && getLocPieceW wplayer destLoc
  where
    (player, wplayer, bplayer) = board
    (P (col, row)) = piece
    (destCol, destRow) = destLoc


getLocPieceW1 :: [Piece] -> Location ->Bool
getLocPieceW1 [] _ = True
getLocPieceW1 (h:t) (col,row) = let (col1,row1) = getPieceLocation h in if col == col1 && row == row1 then False else getLocPieceW1 t (col,row)

getLocPieceB1 :: [Piece] -> Location -> Bool
getLocPieceB1 [] _ = True
getLocPieceB1 (h:t) (col,row) = let (col1,row1) = getPieceLocation h in if col == col1 && row == row1 then False else getLocPieceB1 t (col,row)


legalRookMove :: Board -> Piece -> Location -> Bool
legalRookMove board piece destLoc
  | row >= destRow && col == destCol = checkPath1 board (col, row - 1) destLoc
  | row <= destRow && col == destCol = checkPath2 board (col, row + 1) destLoc
  | row == destRow && col >= destCol = checkPath3 board (pred(col) , row) destLoc
  | row == destRow && col <= destCol = checkPath4 board (succ(col) , row) destLoc
  | otherwise = False
  where
    (player, whitePieces, blackPieces) = board
    (R (col, row)) = piece
    (destCol, destRow) = destLoc


checkPath1 :: Board -> Location -> Location -> Bool
checkPath1 board currloc destLoc
  | player == White && currCol == destCol && currRow == destRow &&  getLocPieceW1 whitePieces destLoc = True
  | player == White && getLocPieceW1 whitePieces currloc && getLocPieceB1 blackPieces currloc = checkPath1 board (currCol, currRow - 1) destLoc
  | player == Black && currCol == destCol && currRow == destRow && getLocPieceB1 blackPieces destLoc = True
  | player == Black && getLocPieceW1 whitePieces currloc && getLocPieceB1 blackPieces currloc = checkPath1 board (currCol, currRow - 1) destLoc
  | otherwise = False
  where
    (player, whitePieces, blackPieces) = board
    (destCol, destRow) = destLoc
    (currCol, currRow) = currloc

checkPath2 :: Board -> Location -> Location -> Bool
checkPath2 board currloc destLoc
  | player == White && currCol == destCol && currRow == destRow&& getLocPieceW1 whitePieces destLoc = True
  | player == White && getLocPieceW1 whitePieces currloc && getLocPieceB1 blackPieces currloc = checkPath2 board (currCol, currRow + 1) destLoc
  | player == Black && currCol == destCol && currRow == destRow && getLocPieceB1 blackPieces destLoc  = True
  | player == Black && getLocPieceW1 whitePieces currloc && getLocPieceB1 blackPieces currloc = checkPath2 board (currCol, currRow + 1) destLoc
  | otherwise = False
  where
    (player, whitePieces, blackPieces) = board
    (destCol, destRow) = destLoc
    (currCol, currRow) = currloc

checkPath3 :: Board -> Location -> Location -> Bool
checkPath3 board currloc destLoc
  | player == White && currCol == destCol && currRow == destRow && getLocPieceW1 whitePieces destLoc = True
  | player == White && getLocPieceW1 whitePieces currloc && getLocPieceB1 blackPieces currloc = checkPath3 board (pred(currCol) , currRow) destLoc
  | player == Black && currCol == destCol && currRow == destRow && getLocPieceB1 blackPieces destLoc  = True
  | player == Black && getLocPieceW1 whitePieces currloc && getLocPieceB1 blackPieces currloc = checkPath3 board (pred(currCol) , currRow) destLoc
  | otherwise = False
  where
    (player, whitePieces, blackPieces) = board
    (destCol, destRow) = destLoc
    (currCol, currRow) = currloc



checkPath4 :: Board -> Location -> Location -> Bool
checkPath4 board currloc destLoc
  | player == White && currCol == destCol && currRow == destRow && getLocPieceW1 whitePieces destLoc = True
  | player == White && getLocPieceW1 whitePieces currloc && getLocPieceB1 blackPieces currloc = checkPath4 board (succ(currCol) , currRow) destLoc
  | player == Black && currCol == destCol && currRow == destRow && getLocPieceB1 blackPieces destLoc = True
  | player == Black && getLocPieceW1 whitePieces currloc && getLocPieceB1 blackPieces currloc = checkPath4 board (succ(currCol) , currRow) destLoc
  | otherwise = False
  where
    (player, whitePieces, blackPieces) = board
    (destCol, destRow) = destLoc
    (currCol, currRow) = currloc

 
legalKnightMove :: Board -> Piece -> Location -> Bool
legalKnightMove board piece (destCol, destRow) = 
  ((((subtractCols col destCol) == 1 && abs (destRow-row) == 2) || (subtractCols col destCol) == 2 && abs (destRow-row) == 1)) && emptySameColor piece destLoc board
  where 
  destLoc = (destCol, destRow) 
  (N (col, row)) = piece


legalBishopMove::Piece ->  Location -> Board-> Bool 
legalBishopMove (B (col,row)) (char,num) (t,l1,l2)
  | notElem char validcol || notElem num validrow=False
  | isFrindelyInFront (pieceLocs pl) (char,num)= False
  | (char,num) `elem` generateBishopLocs (B (col,row)) (x,pl,ol)=True
  |otherwise=False
  where
    x= whichPlayer (B (col,row)) (t,l1,l2)
    pl=whichList (B (col,row)) (t,l1,l2)
    ol=opList (B (col,row)) (t,l1,l2)
legalBishopMove _ x _=False

generateBishopLocs::Piece->Board->[Location]
generateBishopLocs (B (f,r)) (x,pl,ol)= leftUpLocs (f,r) (x,pl,ol) ++ leftDownLocs (f,r) (x,pl,ol) ++ rightUpLocs (f,r) (x,pl,ol) ++ rightDownLocs (f,r) (x,pl,ol)
generateBishopLocs _ _=[]

generateQueenLocs::Piece->Board->[Location]
generateQueenLocs (Q (f,r)) (x,pl,ol)= leftUpLocs (f,r) (x,pl,ol) ++ leftDownLocs (f,r) (x,pl,ol) ++ rightUpLocs (f,r) (x,pl,ol) ++ rightDownLocs (f,r) (x,pl,ol)
generateQueenLocs _ _=[]

leftUpLocs:: Location->Board->[Location]
leftUpLocs ('a',8) _=[]
leftUpLocs (f,r) (x,pl,ol)
  |elem nf validcol && elem nr validrow && notElem (nf,nr) (pieceLocs pl) && notElem (nf,nr) (pieceLocs ol)=(nf,nr):leftUpLocs (nf,nr) (x,pl,ol)
  |elem nf validcol && elem nr validrow && notElem (nf,nr) (pieceLocs pl) && elem (nf,nr) (pieceLocs ol)=[(nf,nr)]
  |otherwise=[]
  where
    nf=intToChar (charToInt f-1)
    nr=r+1

leftDownLocs:: Location->Board->[Location]
leftDownLocs ('a',1) _=[]
leftDownLocs (f,r) (x,pl,ol)
  |elem nf validcol && elem nr validrow && notElem (nf,nr) (pieceLocs pl) && notElem (nf,nr) (pieceLocs ol)=(nf,nr):leftDownLocs (nf,nr) (x,pl,ol)
  |elem nf validcol && elem nr validrow && notElem (nf,nr) (pieceLocs pl) && elem (nf,nr) (pieceLocs ol)=[(nf,nr)]
  |otherwise=[]
  where
    nf=intToChar (charToInt f-1)
    nr=r-1


rightUpLocs:: Location->Board->[Location]
rightUpLocs ('h',8) _=[]
rightUpLocs (f,r) (x,pl,ol)
  |elem nf validcol && elem nr validrow && notElem (nf,nr) (pieceLocs pl) && notElem (nf,nr) (pieceLocs ol)=(nf,nr):rightUpLocs (nf,nr) (x,pl,ol)
  |elem nf validcol && elem nr validrow && notElem (nf,nr) (pieceLocs pl) && elem (nf,nr) (pieceLocs ol)=[(nf,nr)]
  |otherwise=[]
  where
    nf=intToChar (charToInt f+1)
    nr=r+1

rightDownLocs:: Location->Board->[Location]
rightDownLocs ('h',8) _=[]
rightDownLocs (f,r) (x,pl,ol)
  |elem nf validcol && elem nr validrow && notElem (nf,nr) (pieceLocs pl) && notElem (nf,nr) (pieceLocs ol)=(nf,nr):rightDownLocs (nf,nr) (x,pl,ol)
  |elem nf validcol && elem nr validrow && notElem (nf,nr) (pieceLocs pl) && elem (nf,nr) (pieceLocs ol)=[(nf,nr)]
  |otherwise=[]
  where
    nf=intToChar (charToInt f+1)
    nr=r-1

pieceLocs :: [Piece]->[Location]
pieceLocs pieces=[loc | piece <- pieces, let loc = getPieceLocation piece]

isFrindelyInFront :: [Location]->Location->Bool  --in the way*
isFrindelyInFront ls l = l `elem` ls

whichPlayer:: Piece->Board->Player
whichPlayer p (t,wl,bl)
  | p `elem` wl=White
  | otherwise=Black

whichList::Piece->Board->[Piece]
whichList p (t,wl,bl)
  | p `elem` wl=wl
  |otherwise=bl

opList::Piece->Board->[Piece]
opList p (t,wl,bl)
  | p `notElem` wl=wl
  |otherwise=bl

charToInt :: Char -> Int
charToInt c = ord c - ord 'a' + 1

intToChar :: Int -> Char
intToChar i =toEnum (96+i)

validrow :: [Int]
validrow = [1..8]
validcol :: String
validcol=['a'..'h']

legalKingMove :: Board -> Piece -> Location -> Bool
legalKingMove board piece (destCol, destRow)
  | (succ col == destCol || pred col == destCol || col == destCol) && (row - 1 == destRow || row + 1 == destRow || destRow == row) && (destRow <= 8 && destRow >= 1 && destCol >= 'a' && destCol <= 'h') && emptySameColor piece destLoc board = True
  | otherwise = False
  where
    (K (col, row)) = piece
    destLoc = (destCol, destRow)


legalQueenMove :: Board -> Piece -> Location -> Bool
legalQueenMove board piece destLoc 
  | row >= destRow && col == destCol = checkPath1 board (col, row - 1) destLoc
  | row <= destRow && col == destCol = checkPath2 board (col, row + 1) destLoc
  | row == destRow && col >= destCol = checkPath3 board (pred(col) , row) destLoc
  | row == destRow && col <= destCol = checkPath4 board (succ(col) , row) destLoc
  | notElem destCol validcol || notElem destRow validrow=False
  | isFrindelyInFront (pieceLocs whitePieces) (destCol,destRow)= False
  | (destCol,destRow) `elem` generateQueenLocs (Q (col,row)) (x,pl,ol)=True
  | otherwise = False
  where
    (player, whitePieces, blackPieces) = board
    (Q (col, row)) = piece
    (destCol, destRow) = destLoc
    x= whichPlayer (Q (col,row)) (player,whitePieces,blackPieces)
    pl=whichList (Q (col,row)) (player,whitePieces,blackPieces)
    ol=opList (Q (col,row)) (player,whitePieces,blackPieces)



emptySameColor :: Piece -> Location -> Board -> Bool
emptySameColor piece destLoc (player, whitePieces, blackPieces)
  | elem piece whitePieces = if findLoc destLoc whitePieces == Nothing then True else False
  | otherwise = if findLoc destLoc blackPieces == Nothing then True else False


isWhitePiece :: Piece -> Board -> Bool
isWhitePiece piece (_, whitePieces, _) = elem piece whitePieces

subtractCols :: Char -> Char -> Int
subtractCols col1 col2 = abs (colToNum col1 - colToNum col2)
  where
    colToNum :: Char -> Int
    colToNum col = ord col - ord 'a' + 1

---d)
suggestMove :: Piece -> Board -> [Location]
suggestMove piece board = filter (isLegal piece board) allPossibleLocations
  where
    allPossibleLocations = [(c, i) | c <- ['a'..'h'], i <- [1..8]]

---e)
updateboard :: Piece -> Location -> [Piece] -> [Piece]
updateboard _ _ []=[]
updateboard (P l) loc (x:xs)= if P l==x then P loc:updateboard (P l) loc xs
                                           else x:updateboard (P l) loc xs
updateboard (N l) loc (x:xs)= if N l==x then N loc:updateboard (N l) loc xs
                                           else x:updateboard (N l) loc xs
updateboard (K l) loc (x:xs)= if K l==x then K loc:updateboard (K l) loc xs
                                           else x:updateboard (K l) loc xs
updateboard (Q l) loc (x:xs)= if Q l==x then Q loc:updateboard (Q l) loc xs
                                           else x:updateboard (Q l) loc xs
updateboard (R l) loc (x:xs)= if R l==x then R loc:updateboard (R l) loc xs
                                           else x:updateboard (R l) loc xs
updateboard (B l) loc (x:xs)= if B l==x then B loc:updateboard (B l) loc xs
                                           else x:updateboard (B l) loc xs

move:: Piece -> Location -> Board -> Board
move (P l) loc (White, whitel, blackl)| not (isLegal (P l) (White, whitel, blackl) loc) = error $ "Illegal move for piece "++ show (P l)
                                  | P l `notElem` whitel  = error "This is White player's turn, Black can't move."
                                  | otherwise = (Black, newwhite, blackl) where newwhite= updateboard (P l) loc whitel

move (P l) loc (Black, whitel, blackl)| not (isLegal (P l) (Black, whitel, blackl) loc) = error $ "Illegal move for piece: " ++ show (P l)
                                  | P l `notElem` blackl  = error "This is Black player's turn, White can't move."
                                  | otherwise = (White, whitel, newblack) where newblack= updateboard (P l) loc blackl

move (N l) loc (White, whitel, blackl)| not (isLegal (N l) (White, whitel, blackl) loc) = error $ "Illegal move for piece "++ show (N l)
                                  | N l `notElem` whitel  = error "This is White player's turn, Black can't move."
                                  | otherwise = (Black, newwhite, blackl) where newwhite= updateboard (N l) loc whitel

move (N l) loc (Black, whitel, blackl)| not (isLegal (N l) (Black, whitel, blackl) loc) = error $ "Illegal move for piece: " ++ show (N l)
                                  | N l `notElem` blackl  = error "This is Black player's turn, White can't move."
                                  | otherwise = (White, whitel, newblack) where newblack= updateboard (N l) loc blackl
move (K l) loc (White, whitel, blackl)| not (isLegal (K l) (White, whitel, blackl) loc) = error $ "Illegal move for piece "++ show (K l)
                                  | K l `notElem` whitel  = error "This is White player's turn, Black can't move."
                                  | otherwise = (Black, newwhite, blackl) where newwhite= updateboard (K l) loc whitel

move (K l) loc (Black, whitel, blackl)| not (isLegal (K l) (Black, whitel, blackl) loc) = error $ "Illegal move for piece: " ++ show (K l)
                                  | K l `notElem` blackl  = error "This is Black player's turn, White can't move."
                                  | otherwise = (White, whitel, newblack) where newblack= updateboard (K l) loc blackl
move (Q l) loc (White, whitel, blackl)| not (isLegal (Q l) (White, whitel, blackl) loc) = error $ "Illegal move for piece "++ show (Q l)
                                  | Q l `notElem` whitel  = error "This is White player's turn, Black can't move."
                                  | otherwise = (Black, newwhite, blackl) where newwhite= updateboard (Q l) loc whitel

move (Q l) loc (Black, whitel, blackl)| not (isLegal (Q l) (Black, whitel, blackl) loc) = error $ "Illegal move for piece: " ++ show (Q l)
                                  | Q l `notElem` blackl  = error "This is Black player's turn, White can't move."
                                  | otherwise = (White, whitel, newblack) where newblack= updateboard (Q l) loc blackl
move (R l) loc (White, whitel, blackl)| not (isLegal (R l) (White, whitel, blackl) loc) = error $ "Illegal move for piece "++ show (R l)
                                  | R l `notElem` whitel  = error "This is White player's turn, Black can't move."
                                  | otherwise = (Black, newwhite, blackl) where newwhite= updateboard (R l) loc whitel

move (R l) loc (Black, whitel, blackl)| not (isLegal (R l) (Black, whitel, blackl) loc) = error $ "Illegal move for piece: " ++ show (R l)
                                  | R l `notElem` blackl  = error "This is Black player's turn, White can't move."
                                  | otherwise = (White, whitel, newblack) where newblack= updateboard (R l) loc blackl
move (B l) loc (White, whitel, blackl)| not (isLegal (B l) (White, whitel, blackl) loc) = error $ "Illegal move for piece "++ show (B l)
                                  | B l `notElem` whitel  = error "This is White player's turn, Black can't move."
                                  | otherwise = (Black, newwhite, blackl) where newwhite= updateboard (B l) loc whitel

move (B l) loc (Black, whitel, blackl)| not (isLegal (B l) (Black, whitel, blackl) loc) = error $ "Illegal move for piece: " ++ show (B l)
                                  | B l `notElem` blackl  = error "This is Black player's turn, White can't move."
                                  | otherwise = (White, whitel, newblack) where newblack= updateboard (B l) loc blackl



sum_Diagonal m = sum_Diagonal2 m 0
sum_Diagonal2 [] n = 0
sum_Diagonal2 (h:t) n = h!!n + sum_Diagonal2 t n+1