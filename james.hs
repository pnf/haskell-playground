import qualified Data.Map as Map -- refer to function foo as Map.foo


gs = ["g1","g2"]
g2m = Map.fromList [("g1",["m1","m2"]),("g2",["m2"])]
m2w = Map.fromList [("m1",["w1","w2"]),("m2",["w2","w3"])]


nest xs = map (\x -> [x]) xs
-- lookup in dictionary by first element of key list, then prepend to key list
dget :: Ord k => Map.Map k [k] -> [k] -> [[k]]
dget dict ks = map (:ks)  (dict Map.! (head ks))

res =  foldl (>>=) (nest gs) [(dget g2m),(dget m2w)]

manufacturing_groups =   [ "multiglobal manufacturing group",
                         "intergalactic joint ventures",
                         "transdimensional megacorp" ]
manufacturers = Map.fromList [ ("multiglobal manufacturing group", [ "red heron manufacturers",
                                                       "blue bass super-corp",
                                                       "yellow dog llc",
                                                       "orange penguin family manufacturers",
                                                       "green hornet industries"]),
                 ("intergalactic joint ventures", [ "yellow dog llc",
                                                   "purple butterfly inc" ]),
                 ("transdimensional megacorp", [ "maroon baboon manufacturer",
                                                "orange penguin family manufacturers",
                                                "green hornet industries"])]
warehouses = Map.fromList [ ("red heron manufacturers", ["neptune building"]),
               ("blue bass super-corp", ["saturn warehouse"]), 
               ("yellow dog llc", ["mars storage"]), 
               ("orange penguin family manufacturers", ["venus building"]), 
               ("green hornet industries", ["jupiter building"]), 
               ("purple butterfly inc", ["pluto ministorage"]), 
               ("maroon baboon manufacturer", ["mercury warehouse"])]
inventories = Map.fromList [ ("neptune building", ["rose widgets", "petunia widgets"]),
                ("saturn warehouse", ["rose widgets", "daffodil widgets"]),
                ("mars storage", ["poppy widgets", "forget-me-not widgets"]),
                ("venus building", ["goldenrod widgets"]),
                ("jupiter building", ["magnolia widgets"]),
                ("pluto ministorage", ["indian paintbrush widgets", "daffodil widgets"]),
                ("mercury warehouse", ["carnation widgets"])]


res2 = foldl (>>=) (nest manufacturing_groups) (map dget [manufacturers,warehouses,inventories])

