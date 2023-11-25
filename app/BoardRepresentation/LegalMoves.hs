module BoardRepresentation.LegalMoves where
    import Data.Bits
    import BoardRepresentation.Bitboards
    import BoardRepresentation.Moves

    -- Knights
    getKnightMoves :: Bitboard -> Bitboard
    getKnightMoves knightBB = foldr (.|.) 0 [knightBB `shift` x | x <- [-17, -15, -10, -6, 6, 10, 15, 17]]

    getKnightMovesSquare :: Square -> Bitboard
    getKnightMovesSquare = getKnightMoves . squareToBitboard

    -- Generate magic bitboards (we use the Fancy Magic Bitboard method)
    
