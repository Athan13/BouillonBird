module BoardRepresentation.Moves where
    {-
    Squares, plies, and all functions for moving pieces regardless of legality.
    Legality surrounding promotions is covered here (only pawns, only on the 1st/8th rank, not able to promote to kings or pawns)
    -}
    import BoardRepresentation.Bitboards
    import Data.Word (Word64)
    import Data.Bits
    import Data.Char (digitToInt)
    import Data.List (elemIndex)
    import Data.Maybe (fromJust)  -- mostly used to create squares/plies through UCI in GHCi
    
    data Square = Square {file :: Int, rank :: Int}  -- files are columns (a-h), ranks are rows (1-8) represented as 0-7, 0-7
    data Ply = Ply Square Square
                | PromotePly Square Square PieceType
                | NullMove

    instance Show Square where
        show Square {file = file, rank = rank} = fileLetter : show (rank + 1)
            where
                fileLetter = "abcdefgh" !! file

    instance Show Ply where
        show (Ply sq1 sq2)              = show sq1 ++ show sq2
        show (PromotePly sq1 sq2 newPT) = show sq1 ++ show sq2 ++ show newPT
        show NullMove                   = "0000"

    getSquareFromString :: String -> Maybe Square
    getSquareFromString [f1, r1] = case f1 `elemIndex` "abcdefgh" of
        Nothing -> Nothing
        Just fileIndex  -> if 0 <= fileIndex && fileIndex <= 7 && 1 <= rankIndex && rankIndex <= 8 
                            then Just $ Square {file = fileIndex, rank = rankIndex - 1} 
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
            'p' -> Just $ PromotePly sq1 sq2 Pawn
            'n' -> Just $ PromotePly sq1 sq2 Knight
            'b' -> Just $ PromotePly sq1 sq2 Bishop
            'r' -> Just $ PromotePly sq1 sq2 Rook
            'q' -> Just $ PromotePly sq1 sq2 Queen
            'k' -> Just $ PromotePly sq1 sq2 King
            _ -> Nothing
    getPlyFromString _ = Nothing


    -- Square to location on board (0 - 63)
    squareToLocation :: Square -> Int
    squareToLocation Square {file = file, rank = rank} = rank * 8 + (7 - file)

    -- Square to bitboard representing a 1 in the location of the square and a zero elsewhere
    squareToBitboard :: Square -> Word64
    squareToBitboard square = 1 `shiftL` squareToLocation square

    -- Only deals with main move, not promotion
    getPlyBB :: Ply -> Word64
    getPlyBB NullMove = 0
    getPlyBB (Ply sq1 sq2) = squareToBitboard sq1 .|. squareToBitboard sq2
    getPlyBB (PromotePly sq1 _ _) = squareToBitboard sq1
        -- when a pawn promotes, it disappears as a pawn and reappears as a different piece
        -- promotion is handled by makeMove
    
    -- Gets bitboard of piece that is being moved
    getMovePieceBitboard :: Board -> Ply -> Maybe (Bitboard, Piece)
    getMovePieceBitboard _ NullMove = Nothing
    getMovePieceBitboard board (Ply from _) =
        case filter ((/=) 0 . (.&.) square . fst) bitboards of
            [bbp] -> Just bbp
            _ -> Nothing  -- if there are no bitboards with a piece in the starting location
        where
            bitboards = getBoardBitboards board
            square = squareToBitboard from
    getMovePieceBitboard board (PromotePly from to _) = getMovePieceBitboard board (Ply from to)

    makeMove :: Board -> Ply -> Maybe Board
    makeMove board NullMove = Just board
    makeMove board ply@(Ply _ _) =
        case getMovePieceBitboard board ply of
            Nothing -> Nothing
            Just (bitboard, piece) -> Just $ updateBoard board (bitboard `xor` getPlyBB ply, piece) 
    makeMove board pply@(PromotePly from to@(Square {file = _, rank = rank}) newPieceType) =
        case newPieceType of  -- we do not allow promotions to pawns or kings
            Pawn -> Nothing
            King -> Nothing
            _ -> case getMovePieceBitboard board pply of
                Just (bitboard, piece@(Piece colour Pawn)) -> if (colour == White && rank == 7) || (colour == Black && rank == 0)
                                                                then Just $ updateBoard pawnBoard $ firstApply (squareToBitboard to .|.) newPieceBB
                                                                else Nothing  -- only allow promotion on correct rank
                    where
                        pawnBoard  = updateBoard board (bitboard `xor` getPlyBB pply, piece)
                        newPiece   = Piece colour newPieceType
                        newPieceBB = getPieceBitboard board newPiece
                        firstApply f (a, b) = (f a, b)
                _ -> Nothing
            -- We first update the pawn board, then update the promotion board
