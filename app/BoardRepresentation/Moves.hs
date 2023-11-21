module BoardRepresentation.Moves where
    {-
    Squares, plies, and all functions for moving pieces regardless of legality
    -}
    import BoardRepresentation.Bitboards
    import Data.Int
    import Data.Bits

    data Square = Square {rank :: Int64, file :: Int64}
    data Ply = Ply Square Square
                | PromotePly Square Square PieceType
                | NullMove

    -- Square to location in bitboard
    squareToLocation :: Square -> Int64
    squareToLocation Square {rank = rank, file = file} = rank * 7 + file

    -- Only deals with main move, not promotion
    getPlyBB :: Ply -> Int64
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
