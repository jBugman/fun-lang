module Test.Properties
    ( functorIdentity
    , functorCompose
    , monoidAssociativity
    , monoidLeftIdentity
    , monoidRightIdentity
    , semigroupAssociativity

    , exprFunctorIdentity
    , exprFunctorCompose
) where

import ClassyPrelude
import Data.Monoid              (mappend)
import Data.Semigroup           ((<>))
import Test.QuickCheck          (Property, property)
import Test.QuickCheck.Function (Fun (..))

import Fun.SExpression (Atom, Expression)
import Test.Instances  ()

-- S.Expression --

exprFunctorIdentity :: Property
exprFunctorIdentity = property (functorIdentity :: Expression -> Bool)

exprFunctorCompose :: Property
exprFunctorCompose = property
    (functorCompose :: Fun Atom Atom -> Fun Atom Atom -> Expression -> Bool)

-- Generalized --

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Functor f, Eq (f c)) => Fun a b -> Fun b c -> f a -> Bool
functorCompose (Fun _ f) (Fun _ g) x = (fmap g (fmap f x)) == (fmap (g . f) x)

monoidAssociativity :: (Monoid a, Eq a) => a -> a -> a -> Bool
monoidAssociativity x y z = (x `mappend` y) `mappend` z == x `mappend` (y `mappend` z)

monoidLeftIdentity :: (Monoid a, Eq a) => a -> Bool
monoidLeftIdentity x = mempty `mappend` x == x

monoidRightIdentity :: (Monoid a, Eq a) => a -> Bool
monoidRightIdentity x = x `mappend` mempty == x

semigroupAssociativity :: (Semigroup s, Eq s) => s -> s -> s -> Bool
semigroupAssociativity x y z = (x <> y) <> z == x <> (y <> z)
