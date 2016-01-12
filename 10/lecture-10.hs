import Control.Applicative

type Name = String

data Employee = Employee { name    :: Name
                         , phone   :: String }
                deriving Show

-- g `fmap` x === pure g <*> x

m_name1, m_name2 :: Maybe Name
m_name1 = Nothing
m_name2 = Just "Brent"

m_phone1, m_phone2 :: Maybe String
m_phone1 = Nothing
m_phone2 = Just "555-1234"

ex01 = Employee <$> m_name1 <*> m_phone1
ex02 = Employee <$> m_name1 <*> m_phone2
ex03 = Employee <$> m_name2 <*> m_phone1
ex04 = Employee <$> m_name2 <*> m_phone2

{-
u :: f (b -> c)
v :: f (a -> b)
w :: f a

u <*> (v <*> w) :: f (b -> c) -> (f (a -> b) -> f a) -> f c
  = f (b -> c) -> f b -> f c

pure (.) <*> u <*> v <*> w :: 
  ((f ((b -> c) -> (a -> b) -> a -> c) -> f (b -> c)) -> f (a -> b)) -> f a
  = (f ((a -> b) -> a -> c) -> f (a -> b)) -> f a
  = f (a -> c) -> f a
  = f c
-}
