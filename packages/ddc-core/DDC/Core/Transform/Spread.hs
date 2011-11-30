
module DDC.Core.Transform.Spread
        (Spread(..))
where
import DDC.Core.Exp
import DDC.Type.Transform.Spread
import qualified DDC.Type.Check.Env     as Env

instance Spread (Exp a p) where
 spread env xx
  = case xx of
        XVar a u        -> XVar a (spread env u)
        XPrim{}         -> xx
        XCon a u        -> XCon a (spread env u)
        XApp a x1 x2    -> XApp a (spread env x1) (spread env x2)

        XLam a b x      
         -> let b'      = spread env b
            in  XLam a b' (spread (Env.extend b' env) x)
            
        XLet{}          -> error "spread XLet not done"
        XCase{}         -> error "spread XCase not done"
        XCast{}         -> error "spread XCast not done"
        
        XType t         -> XType    (spread env t)
        XWitness w      -> XWitness (spread env w)


instance Spread Witness where
 spread env ww
  = case ww of
        WCon wicon      -> WCon wicon
        WVar n          -> WVar n               -- TODO add type
        WApp  w1 w2     -> WApp  (spread env w1) (spread env w2)
        WJoin w1 w2     -> WJoin (spread env w1) (spread env w2)