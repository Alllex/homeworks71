{- 
	Hometask #5
	Balanced trees
-}

data Tree a = E | Integer a (Tree a) (Tree a)

insert 