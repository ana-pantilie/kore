{- |
Copyright   : (c) Runtime Verification, 2019
License     : NCSA
-}

module Kore.Logger.DebugProofState
    ( DebugProofState
    , debugProofState
    , debugProofStateBefore
    , debugProofStateAfter
    ) where

import Data.Text.Prettyprint.Doc
    ( Pretty (..)
    )
import qualified Data.Text.Prettyprint.Doc as Pretty

import Kore.Internal.TermLike
    ( Variable
    )
import Kore.Logger
import Kore.Step.RulePattern
    ( RulePattern (..)
    )
import Kore.Strategies.ProofState
    ( Prim (..)
    , ProofState (..)
    )

data DebugProofState =
    DebugProofState
        { configuration :: !(Maybe (ProofState (RulePattern Variable)))
        , transition :: !(Maybe (Prim (RulePattern Variable)))
        , result :: !(Maybe (ProofState (RulePattern Variable)))
        , transitionState :: TransitionState
        }

data TransitionState
    = Before
    | After
    | Both
    deriving Eq

instance Pretty DebugProofState where
    pretty
        DebugProofState
            { configuration
            , transition
            , result
            , transitionState
            }
      =
        case transitionState of
            Before ->
                Pretty.vsep
                $ beforeText <> ["...Computing result..."]
            After  ->
                Pretty.vsep afterText
            Both   ->
                Pretty.vsep
                $ beforeText <> afterText
      where
        beforeText =
            [ "Reached proof state with the following configuration:"
            , Pretty.indent 4 (pretty configuration)
            ]
        afterText =
            [ "On which the following transition applies:"
            , Pretty.indent 4 (pretty transition)
            , "Resulting in:"
            , Pretty.indent 4 (pretty result)
            ]

instance Entry DebugProofState where
    entrySeverity _ = Debug

debugProofStateBefore
    :: MonadLog log
    => ProofState (RulePattern Variable)
    -> log ()
debugProofStateBefore config =
    logTransitionState Before (Just config) Nothing Nothing

debugProofStateAfter
    :: MonadLog log
    => Prim (RulePattern Variable)
    -> Maybe (ProofState (RulePattern Variable))
    -> log ()
debugProofStateAfter trans result =
    logTransitionState After Nothing (Just trans) result

debugProofState
    :: MonadLog log
    => ProofState (RulePattern Variable)
    -> Prim (RulePattern Variable)
    -> Maybe (ProofState (RulePattern Variable))
    -> log ()
debugProofState config trans result =
    logTransitionState
        Both
        (Just config)
        (Just trans)
        result

logTransitionState
    :: MonadLog log
    => TransitionState
    -> Maybe (ProofState (RulePattern Variable))
    -> Maybe (Prim (RulePattern Variable))
    -> Maybe (ProofState (RulePattern Variable))
    -> log ()
logTransitionState
    transitionState
    configuration
    transition
    result
  =
    logM DebugProofState
        { configuration
        , transition
        , result
        , transitionState
        }
