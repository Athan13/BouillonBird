module BoardRepresentation.Moves where
    {-
    Squares, plies, and all functions for moving pieces regardless of legality
    -}
    import BoardRepresentation.Bitboards
    import Data.Word (Word64)
    import Data.Bits
    import Data.Char (digitToInt)
    import Data.List (elemIndex)
    import Data.Maybe (fromJust)  -- mostly used to create squares/plies through UCI in GHCi
    
    data Square = Square {file :: Word64, rank :: Word64}  -- ranks are rows (1-8), files are columns (a-h) represented as 0-7, 0-7
    data Ply = Ply Square Square
                | PromotePly Square Square PieceType
                | NullMove

    instance Show Square where
        show Square {file = file, rank = rank} = fileLetter : show (rank + 1)
            where
                fileLetter = "abcdefgh" !! fromIntegral file

    instance Show Ply where
        show (Ply sq1 sq2)              = show sq1 ++ show sq2
        show (PromotePly sq1 sq2 newPT) = show sq1 ++ show sq2 ++ show newPT
        show NullMove                   = "0000"

    getSquareFromString :: String -> Maybe Square
    getSquareFromString [f1, r1] = case f1 `elemIndex` "abcdefgh" of
        Nothing -> Nothing
        Just fileIndex  -> if 0 <= fileIndex && fileIndex <= 7 && 1 <= rankIndex && rankIndex <= 8 
                            then Just $ Square {file = fromIntegral fileIndex, rank = fromIntegral rankIndex - 1} 
                            else Nothing
            where
                rankIndex = digitToInt r1
    getSquareFromString _ = Nothing

    getPlyFromString :: String -> Maybe Ply
    getPlyFromString "0000" = Just NullMove
    getPlyFromString [f1, r1, f2, r2] = case getSquareFromString [f1, r1] of
        Nothing -> Nothing
        Just sq1 -> case getSquareFromString [f2, r2] of
            Nothing -> Nothing
            Just sq2 -> Just $ Ply sq1 sq2
    getPlyFromString [f1, r1, f2, r2, nP] = case getPlyFromString [f1, r1, f2, r2] of
        Nothing -> Nothing
        Just (Ply sq1 sq2)  -> case nP of
            'k' -> Just $ PromotePly sq1 sq2 Knight
            'b' -> Just $ PromotePly sq1 sq2 Bishop
            'r' -> Just $ PromotePly sq1 sq2 Rook
            'q' -> Just $ PromotePly sq1 sq2 Queen
            _ -> Nothing
    getPlyFromString _ = Nothing


    -- Square to location in bitboard
    squareToLocation :: Square -> Word64
    squareToLocation Square {file = file, rank = rank} = (7 - rank) * 8 + file

    -- Only deals with main move, not promotion
    getPlyBB :: Ply -> Word64
    getPlyBB NullMove = 0
    getPlyBB (Ply sq1 sq2) = squareToLocation sq1 .&. squareToLocation sq2
    getPlyBB (PromotePly sq1 _ _) = squareToLocation sq1
        -- when a pawn promotes, it disappears as a pawn and reappears as a different piece
        -- promotion is handled by makeMove
    
    -- Gets bitboard of piece that is being moved
    getMovePieceBitboard :: Board -> Ply -> Maybe (Bitboard, Piece)
    getMovePieceBitboard board (Ply from _) =
        case filter ((/=) 0 . (.&.) square . fst) bitboards of
            [bbp] -> Just bbp
            _ -> Nothing  -- if there are no bitboards with a piece in the starting location
        where
            bitboards = getBoardBitboards board
            square = squareToLocation from

    makeMove :: Board -> Ply -> Maybe Board
    makeMove board NullMove = Just board
    makeMove board ply@(Ply _ _) =
        case getMovePieceBitboard board ply of
            Nothing -> Nothing
            Just (bitboard, piece) -> Just $ updateBoard board (bitboard `xor` getPlyBB ply, piece) 
    makeMove board pply@(PromotePly from to newPieceType) =
        case newPieceType of  -- we do not allow promotions to pawns or kings
            Pawn -> Nothing
            King -> Nothing
            _ -> case getMovePieceBitboard board pply of
                Just (bitboard, Piece colour Pawn) -> case makeMove board (Ply from to) of  -- we only allow pawns to promote
                    Nothing -> Nothing
                    Just pawnBoard -> Just $ updateBoard pawnBoard (newNewPieceBB, newPiece)
                        where
                            newPiece = Piece colour newPieceType
                            existingNewPieceBB = fst $ getPieceBitboard board newPiece
                            newNewPieceBB = existingNewPieceBB .|. squareToLocation to
                _ -> Nothing
            -- We first update the pawn board, then update the promotion board
