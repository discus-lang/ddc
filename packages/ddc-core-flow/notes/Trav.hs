
import Data.Traversable


-- Traversal --------------------------------------------------------
-- Handles: map, fold, unfold, scan.
--
trav :: (a -> s -> (s, b)) -> [a] -> s -> (s, [b])
trav f xx0 s0
 = go xx0 s0
 where  go [] s 
         = (s, [])

        go (x:xs) s
         = let  (s',  y)  = f x s
                (sf,  ys) = go xs s'
           in   (sf, y : ys)


tmap  :: (a -> b) -> [a] -> [b]
tmap f xs
 = snd $ trav f' xs ()
 where  f' x s = (s, f x)


tfold   :: (a -> b -> a) -> a -> [b] -> a
tfold f z xs
 = fst $ trav f' xs z
 where  f' x s = (f s x, ())


tunfold :: (a -> (a, b)) -> a -> [b]
tunfold f z
 = snd $ trav f' (repeat ()) z
 where  f' _ s = f s


tscan   :: (a -> b -> a) -> a -> [b] -> ([a], a)
tscan f z xs
 = (ws, wf)
 where  f' x s   = (f s x, s)
        (wf, ws) = trav f' xs z


-- Segmented Traversal ----------------------------------------------
-- Handles: folds, unfolds, scans.
-- 
travs :: (a -> s -> (s, b)) -> [Int] -> [a] -> [s] -> ([s], [b])
travs f ns xs zs 
 = go ns xs zs [] []
 where  
        go [] _ _ ys' ss'
         = (reverse ss', reverse ys')

        go (0:ns) xs (s:ss) ys' ss'
         = go ns xs ss ys' (s : ss')

        go (n:ns) (x:xs) (s:ss) ys' ss'
         = let  (s', y)  = f x s 
           in   go (n-1 : ns) xs (s':ss) (y : ys') ss'


tfolds   :: (a -> b -> a) -> [Int] -> [a] -> [b] -> [a]
tfolds f ns zs xs
 = fst $ travs f' ns xs zs
 where  f' x s  = (f s x, ())

