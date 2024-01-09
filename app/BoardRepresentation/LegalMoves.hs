module BoardRepresentation.LegalMoves where
    import Data.Bits
    import BoardRepresentation.Bitboards
    import BoardRepresentation.Moves

    getBitboardsOccupancy :: [(Bitboard, Piece)] -> Bitboard
    getBitboardsOccupancy = foldr ((.|.) . fst) 0

    getBoardOccupancy :: Board -> Bitboard
    getBoardOccupancy = getBitboardsOccupancy . getBoardBitboards

    -- Knights
    knightMaskBB :: Bitboard -> Bitboard
    knightMaskBB knightBB = foldr (.|.) 0 [noNoEa, noEaEa, soEaEa, soSoEa, noNoWe, noWeWe, soWeWe, soSoWe]
        where
            noNoEa = (knightBB `shiftL` 17) .&. complement (fileMask 7)
            noEaEa = (knightBB `shiftL` 10) .&. complement (fileMask 7) .&. complement (fileMask 6)
            soEaEa = (knightBB `shiftR` 6)  .&. complement (fileMask 7) .&. complement (fileMask 6)
            soSoEa = (knightBB `shiftR` 15) .&. complement (fileMask 7)
            noNoWe = (knightBB `shiftL` 15) .&. complement (fileMask 1)
            noWeWe = (knightBB `shiftL` 6)  .&. complement (fileMask 1) .&. complement (fileMask 2)
            soWeWe = (knightBB `shiftR` 10) .&. complement (fileMask 1) .&. complement (fileMask 2)
            soSoWe = (knightBB `shiftR` 17) .&. complement (fileMask 1)

    knightMask :: Int -> Bitboard
    knightMask squareI = foldr (.|.) 0 [noNoEa, noEaEa, soEaEa, soSoEa, noNoWe, noWeWe, soWeWe, soSoWe]
        where
            noNoEa = (1 `shift` (squareI + 17)) .&. complement (fileMask 7)
            noEaEa = (1 `shift` (squareI + 10)) .&. complement (fileMask 7) .&. complement (fileMask 6)
            soEaEa = (1 `shift` (squareI - 6))  .&. complement (fileMask 7) .&. complement (fileMask 6)
            soSoEa = (1 `shift` (squareI - 15)) .&. complement (fileMask 7)
            noNoWe = (1 `shift` (squareI + 15)) .&. complement (fileMask 1)
            noWeWe = (1 `shift` (squareI + 6))  .&. complement (fileMask 1) .&. complement (fileMask 2)
            soWeWe = (1 `shift` (squareI - 10)) .&. complement (fileMask 1) .&. complement (fileMask 2)
            soSoWe = (1 `shift` (squareI - 17)) .&. complement (fileMask 1)

    knightMaskSquare :: Square -> Bitboard
    knightMaskSquare = knightMask . squareToLocation

    -- Generate sliding piece attacks (classical approach, magic bitboards are a nightmare)
    -- We start with open board sliding attack bitboards
    rankMask :: Int -> Bitboard
    rankMask squareI = (1 `shiftL` squareI) `xor` (0xff `shiftL` (squareI .&. 56))

    fileMask :: Int -> Bitboard
    fileMask squareI = (1 `shiftL` squareI) `xor` (0x0101010101010101 `shiftL` (squareI .&. 7))

    diagonalMask :: Int -> Bitboard
    diagonalMask squareI = (1 `shiftL` squareI) `xor` if diag > 0 then mainDiag `shiftR` (diag `shiftL` 3) else mainDiag `shiftL` (-1 * (diag `shiftL` 3))
        where
            mainDiag = 0x8040201008040201
            diag = (squareI .&. 7) - (squareI `shiftR` 3)

    antiDiagonalMask :: Int -> Bitboard
    antiDiagonalMask squareI = (1 `shiftL` squareI) `xor` if diag > 0 then mainDiag `shiftR` (diag `shiftL` 3) else mainDiag `shiftL` (-1 * (diag `shiftL` 3))
        where
            mainDiag = 0x0102040810204080
            diag = 7 - (squareI .&. 7) - (squareI `shiftR` 3)


    rookMask :: Int -> Bitboard
    rookMask squareI = rankMask squareI .|. fileMask squareI

    bishopMask :: Int -> Bitboard
    bishopMask squareI = diagonalMask squareI .|. antiDiagonalMask squareI

    queenMask :: Int -> Bitboard
    queenMask squareI = rookMask squareI .|. bishopMask squareI
