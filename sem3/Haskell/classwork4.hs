
data Color = Red|Green|Blue

colors = [Red, Green, Blue]

colorsString x = map (\s -> case s of
                              Red   -> "Red"
                              Green -> "Green"
                              Blue  -> "Blue"
                     ) x                               
                     
data Sound = Sound Int Int

sounds = map (Sound 1) [3,4,5,6] 
inRange (Sound _ j) = j >= 20 && j <= 2000 

data L a = Cons a (L a) | Nil
-- data List a = a ':' (List a) | []

len Nil = 0
len (Cons _ l) = len l + 1