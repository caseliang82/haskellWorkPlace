-- Informatics 1 - Functional Programming
-- Lab week tutorial part II
--
--

import PicturesSVG
import Test.QuickCheck



-- Exercise 9:

pic1 :: Picture
pic1 = above (beside knight (invert knight)) (beside (invert knight) knight)

pic2 :: Picture
pic2 = above (beside knight (invert knight)) (flipV (beside knight (invert knight)))


-- Exercise 10:
-- a)

emptyRow :: Picture
emptyRow = repeatH 4 (beside whiteSquare blackSquare)

-- b)

otherEmptyRow :: Picture
otherEmptyRow = repeatH 4 (beside blackSquare whiteSquare)

-- c)

middleBoard :: Picture
middleBoard = above (above (above emptyRow otherEmptyRow) emptyRow) otherEmptyRow

-- d)

whiteRow :: Picture
whiteRow = beside (beside (beside (beside (beside (beside (beside rook knight) bishop) queen) king) bishop) knight) rook

whitePawn :: Picture
whitePawn = repeatH 8 pawn

blackRow :: Picture
blackRow = invert (whiteRow)

blackPawn :: Picture
blackPawn = invert (whitePawn)

whiteSide :: Picture
whiteSide = above whitePawn whiteRow

blackSide :: Picture
blackSide = above blackRow blackPawn

-- e)

populatedBoard :: Picture
populatedBoard = over (above blackSide (above middleBoard whiteSide)) (above middleBoard middleBoard)



-- Functions --

twoBeside :: Picture -> Picture
twoBeside x = beside x (invert x)


-- Exercise 11:

twoAbove :: Picture -> Picture
twoAbove x = above x (invert x)

fourPictures :: Picture -> Picture
fourPictures x = twoAbove (twoBeside x)
