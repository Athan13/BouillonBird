-- Bitboards plus some admin for displaying the board
module BoardRepresentation.Bitboards where

    import Data.Int
    import Data.Bits
    import Data.Char


    -- Set up datatypes
    type Bitboard = Int64

    data Board = Board {
        whitePawns :: Bitboard,
        blackPawns :: Bitboard,

        whiteKnights :: Bitboard,
        blackKnights :: Bitboard,

        whiteBishops :: Bitboard,
        blackBishops :: Bitboard,

        whiteRooks :: Bitboard,
        blackRooks :: Bitboard,

        whiteQueen :: Bitboard,
        blackQueen :: Bitboard,

        whiteKing :: Bitboard,
        blackKing :: Bitboard
    }

    data PieceColour = White | Black
    data PieceType = Pawn | Knight | Bishop | Rook | Queen | King
    data Piece = Piece PieceColour PieceType | Empty

    newtype BoardArray = BoardArray [[Piece]]

    pieceList :: [PieceType]
    pieceList = [Pawn, Knight, Bishop, Rook, Queen, King]

    getBoardBitboards :: Board -> [(Bitboard, Piece)]
    getBoardBitboards Board {
        whitePawns = wp,
        blackPawns = bp,

        whiteKnights = wn,
        blackKnights = bn,

        whiteBishops = wb,
        blackBishops = bb,

        whiteRooks = wr,
        blackRooks = br,

        whiteQueen = wq,
        blackQueen = bq,

        whiteKing = wk,
        blackKing = bk
    } = [(wp, (Piece White Pawn)), (bp, (Piece Black Pawn)),
         (wn, (Piece White Knight)), (bn, (Piece Black Knight)),
         (wb, (Piece White Bishop)), (bb, (Piece Black Bishop)),
         (wr, (Piece White Rook)), (br, (Piece Black Rook)),
         (wq, (Piece White Queen)), (bq, (Piece Black Queen)),
         (wk, (Piece White King)), (bk, (Piece Black King))]

    instance Show PieceType where
        show Pawn   = "p"
        show Knight = "n"
        show Bishop = "b"
        show Rook   = "r"
        show Queen  = "q"
        show King   = "k"

    instance Show Piece where
        show (Piece White pt) = toUpper <$> show pt
        show (Piece Black pt) = show pt
        show Empty = "_"

    instance Show BoardArray where
        show (BoardArray (ps:[])) = show ps
        show (BoardArray (ps:rest)) = show ps ++ "\n" ++ show (BoardArray rest)
        show _ = ""


    -- Set up starting positions
    startingBitboard :: Piece -> Bitboard
    startingBitboard (Piece White pt) = read hex::Int64 where  -- white pieces
        hex = case pt of
            Pawn   -> "0xff00"
            Knight -> "0x42"
            Bishop -> "0x24"
            Rook   -> "0x81"
            Queen  -> "0x8"
            King   -> "0x10"
    startingBitboard (Piece Black pt) = read hex::Int64 where  -- black pieces
        hex = case pt of
            Pawn   -> "0xff000000000000"
            Knight -> "0x4200000000000000"
            Bishop -> "0x2400000000000000"
            Rook   -> "0x8100000000000000"
            Queen  -> "0x800000000000000"
            King   -> "0x1000000000000000"
    startingBitboard Empty = read "0x0"::Int64

    startingBoard :: Board
    startingBoard = Board {
        whitePawns = startingBitboard (Piece White Pawn),
        blackPawns = startingBitboard (Piece Black Pawn),

        whiteKnights = startingBitboard (Piece White Knight),
        blackKnights = startingBitboard (Piece Black Knight),

        whiteBishops = startingBitboard (Piece White Bishop),
        blackBishops = startingBitboard (Piece Black Bishop),

        whiteRooks = startingBitboard (Piece White Rook),
        blackRooks = startingBitboard (Piece Black Rook),

        whiteQueen = startingBitboard (Piece White Queen),
        blackQueen = startingBitboard (Piece Black Queen),

        whiteKing = startingBitboard (Piece White King),
        blackKing = startingBitboard (Piece Black King)
    }


    -- Displaying the board
    instance Semigroup Piece where
        p1 <> Empty = p1
        Empty <> p2 = p2
        p1 <> p2    = p1

    instance Monoid Piece where
        mempty = Empty
    
    -- From Data.Bits.Bitwise (https://hackage.haskell.org/package/bitwise-1.0.0.1/docs/Data-Bits-Bitwise.html) (couldn't import directly for some reason)
    toListLE :: (Bits b) => b -> [Bool] {- ^ \[least significant bit, ..., most significant bit\] -}
    toListLE b0 | Just n <- bitSizeMaybe b0 = Prelude.map (testBit b0) [0..n-1]
                | otherwise = go b0
        where go b | zeroBits == b = []
                   | otherwise = testBit b 0 : go (b `shiftR` 1)

    getPieceArray :: Bitboard -> Piece -> BoardArray
    getPieceArray bb p = BoardArray [[if bitlist !! (i + j * 8) then p else Empty | i <- [0..7]] | j <- [0..7]]
        where
            bitlist = reverse $ toListLE bb

    instance Semigroup BoardArray where
        (BoardArray ba1) <> (BoardArray ba2) = BoardArray [[(ba1 !! j !! i) <> (ba2 !! j !! i) | i <- [0..7]] | j <- [0..7]]
        
    instance Monoid BoardArray where
        mempty = BoardArray [[Empty | _ <- [0..7]] | _ <- [0..7]]

    getBoardArray :: Board -> BoardArray
    getBoardArray = mconcat . map (uncurry getPieceArray) . getBoardBitboards
