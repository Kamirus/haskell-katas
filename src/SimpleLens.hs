{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SimpleLens where

-- Some functors we will need (implement them)

newtype Identity a = Identity { runIdentity :: a }
  deriving (Eq, Show)

instance Functor Identity where
  fmap f = Identity . f . runIdentity

newtype Const a b = Const { getConst :: a }
  deriving (Eq, Show)

instance Functor (Const a) where
  fmap f = Const . getConst


-- The Lens types (given)

-- source `s`, new source `t`, focus `a`, new focus `b`
type Lens s t a b = forall f . Functor f => (a -> f b) -> (s -> f t)

-- source `s`, focus `a` (focus doesn't change type, so source doesn't change type)
type Lens' s a = Lens s s a a


-- Lens utility functions (implement them – follow the types!)

-- extract the focus `a` from a source `s`
view :: Lens s t a b -> s -> a
view l = getConst . l Const

-- update a focus `a` to `b` within a source `s`, yielding a new source `t`
over :: Lens s t a b -> (a -> b) -> s -> t
over l f = runIdentity . l (Identity . f)

-- set the focus `a` to `b` within a source `s`, yielding a new source `t`
set :: Lens s t a b -> b -> s -> t
set l b = runIdentity . l (Identity . const b)


-- Example lenses (implement them – follow the types!)

-- Tuples

-- a lens focused on the first element of a 2-tuple
_1 :: Lens (a, x) (b, x) a b
_1 f (a, b) = (, b) <$> f a

-- a lens focused on the second element of a 2-tuple
_2 :: Lens (x, a) (x, b) a b
_2 f (a, b) = (a, ) <$> f b

-- Product Types, Records, Etc.

data Person = Person { name :: String, age :: Int }
  deriving (Eq, Show)

-- a lens focused on the name inside a person record
_name :: Lens' Person String
_name f p = (\n -> Person n (age p)) <$> f (name p)


-- Something Fun (inspired by a talk by Simon Peyton Jones)

newtype TempC = TempC { getC :: Float } deriving (Eq, Show, Num)
newtype TempF = TempF { getF :: Float } deriving (Eq, Show, Num)
c_f (TempC c) = TempF $ (9 / 5 * c) + 32
f_c (TempF f) = TempC $ 5 / 9 * (f - 32)

-- the focus doesn't have to be *explicitly* in the source.
-- this lens focuses on the Celsius temp "inside" a Fahrenheit temp.
_celsius :: Lens' TempF TempC
_celsius fc tf = c_f <$> fc (f_c tf)


-- Lens Composition

-- make a lens focused on the name of a person in a nested tuple
-- HINT: this is a very short one-liner. Read the description again!
_1_1_1_name :: Lens' (((Person, x), y), z) String
_1_1_1_name = _1 . _1 . _1 . _name


-- Automatic Lens Generator

-- `lens` can generate a `Lens s t a b` from a getter and setter
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens fget fset f s = t
 where
  t = fset s <$> f a
  a = fget s
