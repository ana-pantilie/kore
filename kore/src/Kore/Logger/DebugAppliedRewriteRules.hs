{- |
Copyright   : (c) Runtime Verification, 2020
License     : NCSA
-}

module Kore.Logger.DebugAppliedRewriteRules
    ( DebugAppliedRewriteRules (..)
    , debugAppliedRewriteRules
    ) where

import Control.Comonad
    ( extract
    )
import Data.Coerce
    ( coerce
    )
import Data.Text.Prettyprint.Doc
    ( Pretty (..)
    )
import qualified Data.Text.Prettyprint.Doc as Pretty

import qualified Kore.Internal.Conditional as Conditional
import Kore.Internal.Variable
    ( Variable (..)
    , toVariable
    )
import Kore.Logger
import Kore.Step.RulePattern
    ( RewriteRule (..)
    , RulePattern (..)
    )
import Kore.Step.Step
    ( UnifiedRule
    , mapRuleVariables
    )
import Kore.Unification.Unify
    ( SimplifierVariable
    )
import Kore.Unparser
    ( unparse
    )
import Kore.Variables.Target
    ( Target (..)
    )

newtype DebugAppliedRewriteRules =
    DebugAppliedRewriteRules
    { appliedRewriteRules
        :: [UnifiedRule Variable (RewriteRule Variable)]
    }

instance Pretty DebugAppliedRewriteRules where
    pretty DebugAppliedRewriteRules { appliedRewriteRules } =
        Pretty.vsep $
            fmap (\x -> unparse . extract $ x) appliedRewriteRules

instance Entry DebugAppliedRewriteRules where
    entrySeverity _ = Debug

debugAppliedRewriteRules
    :: MonadLog log
    => SimplifierVariable variable
    => [UnifiedRule (Target variable) (RulePattern (Target variable))]
    -> log ()
debugAppliedRewriteRules rules =
    logM
    . DebugAppliedRewriteRules
    $ coerce
    (fmap
    (Conditional.mapVariables mapRuleVariables toVariable)
    $ rules
    )
