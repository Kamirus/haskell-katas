{-# LANGUAGE RankNTypes, DeriveFunctor, TupleSections, ScopedTypeVariables, MonomorphismRestriction #-}

module MicroLens where

import           Prelude                 hiding ( sum )
import           Data.Monoid
import           Control.Applicative
import           Data.Tuple
import qualified Data.Traversable              as T

---------------------------------------------------------
-- Some basic libraries

class Profunctor p where
  dimap :: (a' -> a) -> (b -> b') -> (p a b -> p a' b')
  dimap f g = lmap f . rmap g
  lmap ::  (a' -> a) -> (p a b -> p a' b)
  lmap f = dimap f id
  rmap ::  (b -> b') -> (p a b -> p a b')
  rmap f = dimap id f

class Profunctor p => Choice p where
  left'  :: p a b -> p (Either a c) (Either b c)
  right' :: p a b -> p (Either c a) (Either c b)

instance Profunctor (->) where
  dimap f g h = g . h . f

-- left'  :: (a -> b) -> (Either a c) -> (Either b c)
-- right' :: (a -> b) -> (Either c a) -> (Either c b)
instance Choice (->) where
  left' f = either (Left . f) Right
  right' f = either Left (Right . f)

class Contravariant f where
  contramap :: (a' -> a) -> (f a -> f a')

-- Control.Applicative.Const replicated here for your convenience
newtype K b a = K { getK :: b } deriving Functor

instance Monoid b => Applicative (K b) where
  pure _ = K mempty
  K e <*> K f = K (e <> f)

instance Contravariant (K b) where
  contramap f (K b) = K b

newtype Id a = Id { getId :: a } deriving Functor

instance Applicative Id where
  pure = Id
  Id f <*> Id x = Id (f x)

---------------------------------------------------------
-- The lens types you'll implement

-- | Optic is the general pattern for all other lens types.
type Optic p f s t a b = p a (f b) -> p s (f t)

type Iso s t a b = forall p f . (Profunctor p, Functor f) => Optic p f s t a b

type ASetter s t a b = Optic (->) Id s t a b

type Lens s t a b = forall f . Functor f => Optic (->) f s t a b

type Traversal s t a b = forall f . Applicative f => Optic (->) f s t a b

type Fold s a
  = forall f . (Contravariant f, Applicative f) => Optic (->) f s s a a

type Prism s t a b = forall p f . (Choice p, Applicative f) => Optic p f s t a b

---------------------------------------------------------
type Lens_ s t a b = forall f . Functor f => (a -> f b) -> (s -> f t)

-- | A lens focusing on the first element in a pair
_1 :: Lens (a, x) (b, x) a b
_1 f (a, x) = (, x) <$> f a

-- | A lens focusing on the second element in a pair
_2 :: Lens (x, a) (x, b) a b
_2 f (x, a) = (x, ) <$> f a

-- | A function which takes a lens and looks through it.
-- view :: Lens s t a b -> (s -> a)
view :: Optic (->) (K a) s t a b -> (s -> a)
view l = getK . l K

-- | A function which takes a lens and a transformation function
-- and applies that transformer at the focal point of the lens.
-- over :: Lens s t a b -> (a -> b) -> (s -> t)
over :: Optic (->) Id s t a b -> (a -> b) -> (s -> t)
over l f = getId . l (Id . f)

-- | A function from a lens and a value which sets the value
-- at the focal point of the lens. 
-- set :: Lens s t a b -> b -> (s -> t)
set :: Optic (->) Id s t a b -> b -> (s -> t)
set l b = getId . l (Id . const b)

(+~) :: Num a => Lens s t a a -> a -> s -> t
l +~ a = over l (+ a)

-- | Modifies the structure and returns it along with the new value:
(<%~) :: Optic (->) ((,) b) s t a b -> (a -> b) -> s -> (b, t)
l <%~ f = l $ (,) <$> f <*> f -- \a -> (f a, f a)

-- | Modifies the structure and returns it along with the old value:
(<<%~) :: Optic (->) ((,) a) s t a b -> (a -> b) -> s -> (a, t)
l <<%~ f = l $ (,) <$> id <*> f -- \a -> (a, f a)

---------------------------------------------------------

type Traversal_ s t a b = forall f . Applicative f => (a -> f b) -> (s -> f t)

-- | A traversal which focuses on each element in any Traversable container.
elements :: T.Traversable t => Traversal (t a) (t b) a b
elements = traverse

-- | A function which takes a Traversal and pulls out each 
-- element it focuses on in order.
-- 
-- @
-- toListOf :: Traversal s s a a -> (s -> [a])
-- @
toListOf :: Optic (->) (K (Endo [a])) s s a a -> (s -> [a])
toListOf tl = flip appEndo [] . getK . tl f
 where
  f a = K $ Endo $ (:) a

-- | A function which takes any kind of Optic which might
-- be focused on zero subparts and returns Just the first
-- subpart or else Nothing.
--
-- @
-- preview :: Traversal s s a a -> (s -> Maybe a)
-- @
preview :: Optic (->) (K (First a)) s s a a -> (s -> Maybe a)
preview tl = getFirst . getK . tl f
 where
  f :: a -> K (First a) a
  f = K . First . Just

---------------------------------------------------------

-- | A helper function which witnesses the fact that any
-- container which is both a Functor and a Contravariant
-- must actually be empty.
coerce :: forall f a b . (Contravariant f, Functor f) => f a -> f b
coerce = contramap f . fmap f where f = const ()

type Fold_ s a
  = forall f . (Contravariant f, Applicative f) => (a -> f a) -> (s -> f s)

-- | A Fold which views the result of a function application
to :: (a -> b) -> Fold a b
to a_b b_fb = coerce . b_fb . a_b

---------------------------------------------------------

type Prism_ s t a b
  = forall p f . (Choice p, Applicative f) => p a (f b) -> p s (f t)

-- | A prism which focuses on the left branch of an Either
-- p a (f b) -> p (Either a x) (f (Either b x))
-- p a (f b) -> p (Either a x) (Either (f b) x)
_Left :: forall a b x . Prism (Either a x) (Either b x) a b
_Left = rmap (either (fmap Left) (pure . Right)) . left'

-- | A prism which focuses on the right branch of an Either
_Right :: Prism (Either x a) (Either x b) a b
_Right = rmap (either (pure . Left) (fmap Right)) . right'

---------------------------------------------------------

type Iso_ s t a b
  = forall p f . (Profunctor p, Functor f) => p a (f b) -> p s (f t)

-- p (b, a) (f (b, a)) -> p (a, b) (f (a, b))
-- | An iso which witnesses that tuples can be flipped without
-- losing any information
_flip :: Iso (a, b) (a, b) (b, a) (b, a)
_flip = dimap swap $ fmap swap
