module BoardRepresentation.Moves where

    import BoardRepresentation.Bitboards
    import Data.Int
    import Data.Bits

    data Square = Square {rank :: Integer, file :: Integer}
    data Ply = Ply (PieceColour, Square, Square)
                | PromotePly (PieceColour, Square, Square, PieceType)
                | NullMove

    -- Square to location in bitboard
    squareToLocation :: Square -> Integer
    squareToLocation Square {rank = rank, file = file} = rank * 7 + file
    
    -- Gets bitboard of piece that is being moved
    getPieceBitboard :: Board -> Ply -> Bitboard
    getPieceBitboard = undefined

    makeMove :: Board -> Ply -> Board
    makeMove board (Ply (colour, from, to)) = undefined
    makeMove board (PromotePly (colour, from, to, newPiece)) = undefined
    makeMove board NullMove = board
