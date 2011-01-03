
import Util.Graph.Deps

import qualified Data.Map as Map
import qualified Data.Set as Set

main :: IO ()
main
 = do
	putStrLn $ "\nAcyclic graph: " ++ show (Map.toList acyclic)
	putStrLn $ "reachable     0 : " ++ show (Set.toList $ graphReachable acyclic [0])
	putStrLn $ "reachable     1 : " ++ show (Set.toList $ graphReachable acyclic [1])
	putStrLn $ "reachable1_nr 0 : " ++ show (Set.toList $ graphReachable1_nr acyclic 0)
	putStrLn $ "reachable1_nr 1 : " ++ show (Set.toList $ graphReachable1_nr acyclic 1)
	putStrLn $ "\nCyclic graph : " ++ show (Map.toList cyclic)
	putStrLn $ "reachable     1 : " ++ show (Set.toList $ graphReachable cyclic [1])
	putStrLn $ "reachable     2 : " ++ show (Set.toList $ graphReachable cyclic [2])
	putStrLn $ "reachable1_nr 1 : " ++ show (Set.toList $ graphReachable1_nr cyclic 1)
	putStrLn $ "reachable1_nr 2 : " ++ show (Set.toList $ graphReachable1_nr cyclic 2)
	putStrLn ""


acyclic :: Map.Map Int [Int]
acyclic = Map.fromList
	[ (1, [2, 3])
	, (2, [3])
	, (3, [7])
	, (4, [5])
	, (5, [6])
	, (6, [7])
	, (7, [8])
	, (8, [])
	, (0, [1, 2, 4]) ]


cyclic :: Map.Map Int [Int]
cyclic = Map.fromList
	[ (1, [2, 3])
	, (2, [3])
	, (3, [2]) ]

