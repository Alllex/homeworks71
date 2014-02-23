
data Zero
data Succ a

class Add a b c | a b -> c where
	add :: a -> b -> c

zero :: Zero
zero = undefined

one :: Succ Zero
one = undefined

two :: Succ (Succ Zero)
two = undefined

instance Add Zero a a
instance Add a b c => Add (Succ a) b (Succ c)
 
class Mul a b c | a b -> c where
	mul :: a -> b -> c

instance Mul Zero a Zero
instance (Mul a b c, Add c b d) => Mul (Succ a) b d