{- |
Copyright   : (c) Runtime Verification, 2019
License     : NCSA

Target specific variables for unification.

 -}

module Kore.Variables.Target
    ( Target (..)
    , unwrapVariable
    , isTarget
    , isNonTarget
    ) where

import Data.Hashable
    ( Hashable
    )
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC

import Kore.Debug
import Kore.Syntax.Variable
    ( SortedVariable (..)
    )
import Kore.Unparser
    ( Unparse (..)
    )
import Kore.Variables.Fresh
    ( FreshVariable (..)
    )

{- | Distinguish variables by their source.

'Target' variables always compare 'LT' 'NonTarget' variables, so that the
unification procedure prefers to generate substitutions for 'Target' variables
instead of 'NonTarget' variables.

 -}
data Target variable
    = Target !variable
    | NonTarget !variable
    deriving (Eq, GHC.Generic, Show, Functor)

instance Ord variable => Ord (Target variable) where
    compare (Target var1) (Target var2) = compare var1 var2
    compare (Target _) (NonTarget _) = LT
    compare (NonTarget var1) (NonTarget var2) = compare var1 var2
    compare (NonTarget _) (Target _) = GT

instance Hashable variable => Hashable (Target variable)

instance SOP.Generic (Target variable)

instance SOP.HasDatatypeInfo (Target variable)

instance Debug variable => Debug (Target variable)

instance (Debug variable, Diff variable) => Diff (Target variable)

unwrapVariable :: Target variable -> variable
unwrapVariable (Target variable) = variable
unwrapVariable (NonTarget variable) = variable

isTarget :: Target variable -> Bool
isTarget (Target _) = True
isTarget (NonTarget _) = False

isNonTarget :: Target variable -> Bool
isNonTarget = not . isTarget

instance
    SortedVariable variable
    => SortedVariable (Target variable)
  where
    fromVariable = Target . fromVariable
    toVariable (Target variable) = toVariable variable
    toVariable (NonTarget variable) = toVariable variable

{- | Ensures that fresh variables are unique under 'unwrapStepperVariable'.
 -}
instance FreshVariable variable => FreshVariable (Target variable)

instance
    Unparse variable =>
    Unparse (Target variable)
  where
    unparse (Target var) = unparse var
    unparse (NonTarget var) = unparse var
    unparse2 (Target var) = unparse2 var
    unparse2 (NonTarget var) = unparse2 var
