module Test.Properties
    ( monofunctorIdentity
    , monofunctorCompose
) where

import           Data.MonoTraversable
import           Data.Text                (Text)
import           Prelude                  (Bool, Eq, id, (.), (==))
import           Test.QuickCheck
import qualified Test.QuickCheck.Function as QC

import qualified Fun.Sexp       as S
import           Test.Instances ()


monofunctorIdentity :: Property
monofunctorIdentity = property (prop :: S.Expression Text -> Bool)
    where
        prop :: (MonoFunctor f, Eq f) => f -> Bool
        prop f = omap id f == f

monofunctorCompose :: Property
monofunctorCompose = property (prop :: QC.Fun Text Text -> QC.Fun Text Text -> S.Expression Text -> Bool)
    where
        prop :: (MonoFunctor f, Eq f) => QC.Fun (Element f) (Element f) -> QC.Fun (Element f) (Element f) -> f -> Bool
        prop (QC.Fun _ f) (QC.Fun _ g) x = (omap g (omap f x)) == (omap (g . f) x)
