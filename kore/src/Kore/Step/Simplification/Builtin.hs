{- |
Copyright   : (c) Runtime Verification, 2019
License     : NCSA

 -}
module Kore.Step.Simplification.Builtin
    ( simplify
    ) where

import qualified Kore.Domain.Builtin as Domain
import           Kore.Internal.Conditional
                 ( Conditional )
import           Kore.Internal.MultiOr as MultiOr
import           Kore.Internal.OrPattern
                 ( OrPattern )
import           Kore.Internal.Pattern
                 ( Pattern (..) )
import           Kore.Internal.TermLike
import           Kore.Unparser

{-| 'simplify' simplifies a 'DomainValue' pattern, which means returning
an or containing a term made of that value.
-}
simplify
    :: ( Ord variable
       , Show variable
       , Unparse variable
       , SortedVariable variable
       )
    => Builtin (OrPattern variable)
    -> OrPattern variable
simplify builtin =
    MultiOr.filterOr $ do
        child <- simplifyBuiltin builtin
        return . Pattern $ (mkBuiltin <$> child)

simplifyBuiltin
    :: ( Ord variable
       , Show variable
       , Unparse variable
       , SortedVariable variable
       )
    => Builtin (OrPattern variable)
    -> MultiOr (Conditional variable (Builtin (TermLike variable)))
simplifyBuiltin =
    \case
        Domain.BuiltinMap _map -> do
            _map <- sequence _map
            -- MultiOr propagates \bottom children upward.
            return (Domain.BuiltinMap <$> sequenceA (unPattern <$> _map))
        Domain.BuiltinList _list -> do
            _list <- sequence _list
            -- MultiOr propagates \bottom children upward.
            return (Domain.BuiltinList <$> sequenceA (unPattern <$> _list))
        Domain.BuiltinSet _set -> do
            _set <- sequence _set
            return (Domain.BuiltinSet <$> sequenceA (unPattern <$> _set))
        Domain.BuiltinInt int -> (return . pure) (Domain.BuiltinInt int)
        Domain.BuiltinBool bool -> (return . pure) (Domain.BuiltinBool bool)
        Domain.BuiltinString string ->
            (return . pure) (Domain.BuiltinString string)
