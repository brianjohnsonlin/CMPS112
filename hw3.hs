{-
 - Program: hw3.hs
 - Authors: Yunyi Ding and Brian Lin
 - On this homework, we worked together for 10 hours,
 - Yunyi worked independently for 5 hours,
 - and Brian worked independently for 5 hours.
-}

data BST k v = Empty | Node k v (BST k v) (BST k v)

val :: BST k v -> Maybe v
val Empty = Nothing
val (Node _ v _ _) = Just v

size :: BST k v -> Int
size Empty = 0
size (Node _ v l r) = 1 + size l + size r

ins :: (Ord k) => k -> v -> BST k v -> BST k v
ins k v Empty = Node k v Empty Empty
ins k v (Node k' v' l r)
	| k == k' = Node k v l r
	| k < k' = Node k' v' (ins k v l) r
	| k > k' = Node k' v' l (ins k v r)

instance (Show v) => Show (BST k v) where
	show Empty = ""
	show (Node k v left right) = "(" ++ show left ++ show v ++ show right ++ ")"

data JSON = JStr String --JavaScript Object Notation
          | JNum Double
          | JArr [JSON]
          | JObj [(String, JSON)]

instance Show JSON where
	show (JStr j) = show j
	show (JNum j) = show j
	show (JArr j) = show j
	show (JObj j) = show j


class Json a where
	toJson :: a -> JSON
	fromJson :: JSON -> a

instance Json Double where
	toJson d = JNum d
	fromJson (JNum d) = d

instance (Json a) => Json [a] where
	toJson l = JArr (map toJson l)
	fromJson (JArr l) = map fromJson l
