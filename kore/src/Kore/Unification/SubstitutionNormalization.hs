{-|
Module      : Kore.Unification.SubstitutionNormalization
Description : Normalization for substitutions resulting from unification, so
              that they can be safely used on the unified term.
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable
-}
module Kore.Unification.SubstitutionNormalization
    ( normalizeSubstitution
    ) where

import qualified Control.Comonad.Trans.Cofree as Cofree
import Control.Monad.Except
    ( ExceptT (..)
    , throwError
    )
import qualified Control.Monad.State.Strict as State
import qualified Data.Foldable as Foldable
import Data.Functor.Const
import Data.Functor.Foldable
    ( Base
    )
import qualified Data.Functor.Foldable as Recursive
import Data.Map.Strict
    ( Map
    )
import qualified Data.Map.Strict as Map
import Data.Maybe
    ( mapMaybe
    )
import Data.Set
    ( Set
    )
import qualified Data.Set as Set

import Data.Graph.TopologicalSort
import qualified Kore.Attribute.Pattern.FreeVariables as FreeVariables
import Kore.Internal.Predicate
    ( Predicate
    )
import qualified Kore.Internal.Predicate as Predicate
import qualified Kore.Internal.Symbol as Symbol
import Kore.Internal.TermLike as TermLike
import Kore.Step.Simplification.Simplify
    ( MonadSimplify
    , SimplifierVariable
    )
import Kore.TopBottom
import Kore.Unification.Error
    ( SubstitutionError (..)
    )
import qualified Kore.Unification.Substitution as Substitution
import Kore.Variables.UnifiedVariable
import qualified Kore.Variables.UnifiedVariable as UnifiedVariable

data TopologicalSortResult variable
  = MixedCtorCycle ![variable]
  | SetCtorCycle ![variable]
  | Sorted ![variable]
  deriving (Show)

{-| 'normalizeSubstitution' transforms a substitution into an equivalent one
in which no variable that occurs on the left hand side also occurs on the
right side.

The substitution is presented as a 'Map' where any duplication has already been
resolved.

Returns an error when the substitution is not normalizable (i.e. it contains
x = f(x) or something equivalent).
-}
normalizeSubstitution
    :: forall m variable
    .  (MonadSimplify m, SimplifierVariable variable)
    => Map (UnifiedVariable variable) (TermLike variable)
    -> ExceptT SubstitutionError m (Predicate variable)
normalizeSubstitution = normalizeSubstitution'

normalizeSubstitution'
    :: forall m variable
    .  (MonadSimplify m, SimplifierVariable variable)
    => Map (UnifiedVariable variable) (TermLike variable)
    -> ExceptT SubstitutionError m (Predicate variable)
normalizeSubstitution' substitution = do
    let
        -- | Do a `topologicalSort` of variables using the `dependencies` Map.
        -- Topological cycles with non-ctors are returned as Left errors.
        -- Non-simplifiable cycles are returned as Right Nothing.
        topologicalSortConverted
            :: Either
                SubstitutionError
                (TopologicalSortResult (UnifiedVariable variable))
        topologicalSortConverted =
            case topologicalSort (Set.toList <$> allDependencies) of
                Left (TopologicalSortCycles vars) ->
                    case nonSimplifiableSortResult of
                        Left (TopologicalSortCycles nonSimplifiableCycle)
                          | all isVariable
                            (mapMaybe
                                (`Map.lookup` substitution)
                                nonSimplifiableCycle
                            ) ->
                            error
                                (  "Impossible: order on variables should "
                                ++ "prevent only-variable-cycles"
                                )
                          | all UnifiedVariable.isSetVar nonSimplifiableCycle ->
                            Right (SetCtorCycle nonSimplifiableCycle)
                          | otherwise ->
                            Right (MixedCtorCycle nonSimplifiableCycle)
                        Right _ ->
                            Left (NonCtorCircularVariableDependency vars)
                Right result -> Right (Sorted result)
          where
            nonSimplifiableSortResult =
                topologicalSort (Set.toList <$> nonSimplifiableDependencies)
    case topologicalSortConverted of
        Left err -> throwError err
        Right (MixedCtorCycle _) -> return Predicate.bottom
        Right (Sorted vars) -> return $ normalize' vars
        Right (SetCtorCycle vars) ->
            let substitution' = Foldable.foldl' assignBottom substitution vars
            in normalizeSubstitution' substitution'
  where
    isVariable :: TermLike variable -> Bool
    isVariable term =
        case Cofree.tailF (Recursive.project term) of
            VariableF _ -> True
            _ -> False

    assignBottom
        :: Map (UnifiedVariable variable) (TermLike variable)
        -> UnifiedVariable variable
        -> Map (UnifiedVariable variable) (TermLike variable)
    assignBottom subst variable =
        Map.adjust (mkBottom . termLikeSort) variable subst

    interestingVariables :: Set (UnifiedVariable variable)
    interestingVariables = Map.keysSet substitution

    allDependencies
        :: Map (UnifiedVariable variable) (Set (UnifiedVariable variable))
    allDependencies =
        Map.mapWithKey
            (getDependencies interestingVariables)
            substitution

    nonSimplifiableDependencies
        :: Map (UnifiedVariable variable) (Set (UnifiedVariable variable))
    nonSimplifiableDependencies =
        Map.mapWithKey
            (getNonSimplifiableDependencies interestingVariables)
            substitution

    sortedSubstitution
        :: [UnifiedVariable variable]
        -> [(UnifiedVariable variable, TermLike variable)]
    sortedSubstitution = fmap (variableToSubstitution substitution)

    normalize'
        :: [UnifiedVariable variable]
        -> Predicate variable
    normalize' order
      | any (not . isSatisfiableSubstitution) sorted = Predicate.bottom
      | otherwise =
        Predicate.fromSubstitution . Substitution.unsafeWrap
        $ backSubstitute sorted
      where
        sorted = sortedSubstitution order

variableToSubstitution
    :: (Ord variable, Show variable)
    => Map (UnifiedVariable variable) (TermLike variable)
    -> UnifiedVariable variable
    -> (UnifiedVariable variable, TermLike variable)
variableToSubstitution varToPattern var =
    case Map.lookup var varToPattern of
        Just patt -> (var, patt)
        Nothing   -> error ("variable " ++ show var ++ " not found.")

{- | Apply a topologically sorted list of substitutions to itself.

Apply the substitutions in order so that finally no substitution in the list
depends on any other. The substitution must be topologically sorted.

The post-condition of this function depends on the following pre-conditions,
which are not enforced:
No substitution variable may appear in its own assignment.
Every substitution must be satisfiable, see 'isSatisfiableSubstitution'.

 -}
backSubstitute
    :: forall variable
    .  SimplifierVariable variable
    => [(UnifiedVariable variable, TermLike variable)]
    -- ^ Topologically-sorted substitution
    -> [(UnifiedVariable variable, TermLike variable)]
backSubstitute sorted =
    (flip State.evalState mempty) (traverse worker sorted)
  where
    worker (variable, termLike) = do
        termLike' <- applySubstitution termLike
        insertSubstitution variable termLike'
        return (variable, termLike')
    insertSubstitution variable termLike =
        State.modify' $ Map.insert variable termLike
    applySubstitution termLike = do
        substitution <- State.get
        return $ TermLike.substitute substitution termLike

isTrivialSubstitution
    :: Eq variable
    => (UnifiedVariable variable, TermLike variable)
    -> Bool
isTrivialSubstitution (variable, Var_ variable') = variable == variable'
isTrivialSubstitution _ = False

dropTrivialSubstitutions
    :: Eq variable
    => [(UnifiedVariable variable, TermLike variable)]
    -> [(UnifiedVariable variable, TermLike variable)]
dropTrivialSubstitutions = filter (not . isTrivialSubstitution)

isSatisfiableSubstitution
    :: (UnifiedVariable variable, TermLike variable)
    -> Bool
isSatisfiableSubstitution (variable, termLike) =
    not $ isElemVar variable && isBottom termLike

{- | Calculate the dependencies of a substitution.

    Calculate the interesting dependencies of a substitution. The interesting
    dependencies are interesting variables that are free in the substitution
    pattern.
 -}
getDependencies
    :: (Ord variable, Show variable)
    => Set (UnifiedVariable variable)  -- ^ interesting variables
    -> UnifiedVariable variable  -- ^ substitution variable
    -> TermLike variable  -- ^ substitution pattern
    -> Set (UnifiedVariable variable)
getDependencies interesting var termLike =
    case termLike of
        Var_ v | v == var -> Set.empty
        _ -> Set.intersection interesting freeVars
  where
    freeVars = FreeVariables.getFreeVariables $ TermLike.freeVariables termLike

{- | Calculate the dependencies of a substitution that have only
     non-simplifiable symbols above.

    Calculate the interesting dependencies of a substitution. The interesting
    dependencies are interesting variables that are free in the substitution
    pattern.
 -}
getNonSimplifiableDependencies
    ::  ( Ord variable
        , Show variable
        )
    => Set (UnifiedVariable variable)  -- ^ interesting variables
    -> UnifiedVariable variable  -- ^ substitution variable
    -> TermLike variable  -- ^ substitution pattern
    -> Set (UnifiedVariable variable)
getNonSimplifiableDependencies interesting var termLike =
    case termLike of
        Var_ v | v == var -> Set.empty
        _ -> Recursive.fold (nonSimplifiableAbove interesting) termLike

nonSimplifiableAbove
    :: forall variable. (Ord variable, Show variable)
    => Set (UnifiedVariable variable)
    -> Base (TermLike variable) (Set (UnifiedVariable variable))
    -> Set (UnifiedVariable variable)
nonSimplifiableAbove interesting p =
    case Cofree.tailF p of
        VariableF (Const v)
            | v `Set.member` interesting -> Set.singleton v
        ApplySymbolF Application { applicationSymbolOrAlias }
            | Symbol.isNonSimplifiable applicationSymbolOrAlias ->
                dependencies
        _ -> Set.empty
  where
    dependencies :: Set (UnifiedVariable variable)
    dependencies = foldl Set.union Set.empty p
