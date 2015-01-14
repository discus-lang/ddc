

{-
data Env b u
        = Env
        { envMake       :: [(b, Exp a b u k)] -> Env b u
        , envApp        :: Env b u -> Env b u -> Env b u
        , envLookup     :: Env b u -> u       -> Maybe (Exp b u) }



-}
