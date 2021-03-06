{- |
Copyright   : (c) Runtime Verification, 2020
License     : NCSA

-}

module Kore.Log.WarnRetrySolverQuery
    ( WarnRetrySolverQuery
    , warnRetrySolverQuery
    ) where

import Prelude.Kore

import Kore.Internal.Predicate
    ( Predicate
    )
import qualified Kore.Internal.Predicate as Predicate
import Kore.Internal.Variable
    ( InternalVariable
    , VariableName
    , toVariableName
    )
import Kore.Unparser
    ( unparse
    )
import Log
import Pretty
    ( Pretty (..)
    )
import qualified Pretty

newtype WarnRetrySolverQuery =
    WarnRetrySolverQuery
        { predicates :: NonEmpty (Predicate VariableName) }
    deriving (Show)

instance Pretty WarnRetrySolverQuery where
    pretty WarnRetrySolverQuery { predicates } =
        Pretty.vsep $
        [ "The SMT solver initially failed to solve the following query:"
        , Pretty.indent 2 "Decide predicate:"
        , Pretty.indent 4 (unparse predicate)
        , Pretty.indent 2 "with side conditions:"
        ]
        <> fmap (Pretty.indent 4 . unparse) sideConditions
        <> ["The SMT solver was reset and the query\
            \ was tried one more time."
           ]
      where
        predicate :| sideConditions = predicates

instance Entry WarnRetrySolverQuery where
    entrySeverity _ = Warning
    helpDoc _ =
        "warning raised when the solver failed to decide\
        \ the satisfiability of a formula, indicating that\
        \ the solver was reset and the formula retried"

warnRetrySolverQuery
    :: InternalVariable variable
    => MonadLog log
    => NonEmpty (Predicate variable)
    -> log ()
warnRetrySolverQuery predicates' =
    logEntry WarnRetrySolverQuery { predicates }
  where
    predicates =
        Predicate.mapVariables (pure toVariableName) <$> predicates'
