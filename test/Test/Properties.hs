module Test.Properties
    ( monofunctorIdentity
    , monofunctorCompose
    , monoidAssociativity
    , monoidLeftIdentity
    , semigroupAssociativity
) where

import           ClassyPrelude
import           Data.Monoid              (mappend)
import           Data.Semigroup           ((<>))
import           Test.QuickCheck
import qualified Test.QuickCheck.Function as QC

import qualified Fun.Sexp       as S
import           Test.Instances ()


type SE = S.Expression

monofunctorIdentity :: Property
monofunctorIdentity = property (prop :: SE -> Bool)
    where
        prop :: (MonoFunctor f, Eq f) => f -> Bool
        prop f = omap id f == f

monofunctorCompose :: Property
monofunctorCompose = property (prop :: QC.Fun Text Text -> QC.Fun Text Text -> SE -> Bool)
    where
        prop :: (MonoFunctor f, Eq f) => QC.Fun (Element f) (Element f) -> QC.Fun (Element f) (Element f) -> f -> Bool
        prop (QC.Fun _ f) (QC.Fun _ g) x = (omap g (omap f x)) == (omap (g . f) x)

monoidAssociativity :: Property
monoidAssociativity = property (prop :: SE -> SE -> SE -> Bool)
    where
        prop x y z = (x `mappend` y) `mappend` z == x `mappend` (y `mappend` z)

monoidLeftIdentity :: Property
monoidLeftIdentity = property (prop :: SE -> Bool)
    where
        prop x = mempty `mappend` x == x

semigroupAssociativity :: Property
semigroupAssociativity = property (prop :: SE -> SE -> SE -> Bool)
    where
        prop x y z = (x <> y) <> z == x <> (y <> z)
