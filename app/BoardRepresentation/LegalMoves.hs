module BoardRepresentation.LegalMoves where
    import Data.Bits
    import BoardRepresentation.Bitboards
    import BoardRepresentation.Moves

    getBoardOccupancy :: Board -> Bitboard
    getBoardOccupancy = foldr ((.|.) . fst) 0 . getBoardBitboards

    -- Knights
    fileMaskFull :: Int -> Bitboard
    fileMaskFull squareI = 0x0101010101010101 `shiftL` (squareI .&. 7)

    knightMaskBB :: Bitboard -> Bitboard
    knightMaskBB knightBB = foldr (.|.) 0 [noNoEa, noEaEa, soEaEa, soSoEa, noNoWe, noWeWe, soWeWe, soSoWe]
        where
            noNoEa = (knightBB `shiftL` 15) .&. complement (fileMaskFull 7)
            noEaEa = (knightBB `shiftL` 6)  .&. complement (fileMaskFull 7) .&. complement (fileMaskFull 6)
            soEaEa = (knightBB `shiftR` 10) .&. complement (fileMaskFull 7) .&. complement (fileMaskFull 6)
            soSoEa = (knightBB `shiftR` 17) .&. complement (fileMaskFull 7)
            noNoWe = (knightBB `shiftL` 17) .&. complement (fileMaskFull 0)
            noWeWe = (knightBB `shiftL` 10) .&. complement (fileMaskFull 0) .&. complement (fileMaskFull 1)
            soWeWe = (knightBB `shiftR` 6)  .&. complement (fileMaskFull 0) .&. complement (fileMaskFull 1)
            soSoWe = (knightBB `shiftR` 15) .&. complement (fileMaskFull 0)

    knightMask :: Int -> Bitboard
    knightMask squareI = foldr (.|.) 0 [noNoEa, noEaEa, soEaEa, soSoEa, noNoWe, noWeWe, soWeWe, soSoWe]
        where
            noNoEa = (1 `shift` (squareI + 15)) .&. complement (fileMaskFull 7)
            noEaEa = (1 `shift` (squareI + 6))  .&. complement (fileMaskFull 7) .&. complement (fileMaskFull 6)
            soEaEa = (1 `shift` (squareI - 10)) .&. complement (fileMaskFull 7) .&. complement (fileMaskFull 6)
            soSoEa = (1 `shift` (squareI - 17)) .&. complement (fileMaskFull 7)
            noNoWe = (1 `shift` (squareI + 17)) .&. complement (fileMaskFull 0)
            noWeWe = (1 `shift` (squareI + 10)) .&. complement (fileMaskFull 0) .&. complement (fileMaskFull 1)
            soWeWe = (1 `shift` (squareI - 6))  .&. complement (fileMaskFull 0) .&. complement (fileMaskFull 1)
            soSoWe = (1 `shift` (squareI - 15)) .&. complement (fileMaskFull 0)

    knightMaskSquare :: Square -> Bitboard
    knightMaskSquare = knightMask . squareToLocation

    -- Kings
    kingMaskBB :: Bitboard -> Bitboard
    kingMaskBB bb = sideAttacks .|. (sideAttacks `shiftL` 8) .|. (sideAttacks `shiftR` 8)
        where
            sideAttacks = lAttack .|. bb .|. rAttack
            lAttack = bb `shiftL` 1 .&. complement (fileMaskFull 7)
            rAttack = bb `shiftR` 1 .&. complement (fileMaskFull 0)

    kingMask :: Int -> Bitboard
    kingMask squareI = sideAttacks .|. (sideAttacks `shiftL` 8) .|. (sideAttacks `shiftR` 8)
        where
            sideAttacks = lAttack .|. (1 `shiftL` squareI) .|. rAttack
            lAttack = 1 `shift` (squareI - 1) .&. complement (fileMaskFull 7)
            rAttack = 1 `shift` (squareI + 1) .&. complement (fileMaskFull 0)


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
