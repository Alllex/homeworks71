infixl 6 +++ 
class SemiGroup a where
	(+++) :: a -> a -> a

class SemiGroup a => Monoid a where
	e :: a

class Monoid a => Group a where
	inv :: a -> a

infixl 7 ***
class Group a => Ring a where
	(***) :: a -> a -> a

class Ring a => RingE a where
	eM :: a

class RingE a => Field a where
	invM :: a -> a

instance SemiGroup Int where
	x +++ y = x + y

instance Monoid Int where
	e = 0

instance Group Int where
	inv = (-) 0
