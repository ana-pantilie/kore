{-|
Module      : Kore.Repl.Data
Description : REPL data structures.
Copyright   : (c) Runtime Verification, 2019
License     : NCSA
Maintainer  : vladimir.ciobanu@runtimeverification.com
-}

module Kore.Repl.State
    ( emptyExecutionGraph
    , getClaimByIndex, getAxiomByIndex, getAxiomOrClaimByIndex
    , getInternalIdentifier
    , getAxiomByName, getClaimByName, getAxiomOrClaimByName
    , getClaimIndexByName, getNameText
    , ruleReference
    , switchToProof
    , getTargetNode, getInnerGraph, getExecutionGraph
    , smoothOutGraph
    , getConfigAt, getRuleFor, getLabels, setLabels
    , runStepper, runStepper'
    , runUnifier
    , updateInnerGraph, updateExecutionGraph
    , addOrUpdateAlias, findAlias, substituteAlias
    , sortLeafsByType
    , generateInProgressClaims
    , currentClaimSort
    , conjOfClaims
    , appReplOut
    , replOut, replOutputToString
    , allProofs
    ) where

import Prelude.Kore

import Control.Concurrent.MVar
import qualified Control.Lens as Lens
import Control.Monad.Error.Class
    ( MonadError
    )
import qualified Control.Monad.Error.Class as Monad.Error
import Control.Monad.State.Strict
    ( MonadState
    , get
    , modify
    )
import qualified Control.Monad.Trans.Class as Monad.Trans
import qualified Data.Bifunctor as Bifunctor
import Data.Bitraversable
    ( bisequence
    , bitraverse
    )
import Data.Coerce
    ( coerce
    )
import qualified Data.Default as Default
import Data.Foldable
    ( find
    )
import Data.Generics.Product
import qualified Data.Graph.Inductive.Graph as Graph
import Data.Graph.Inductive.PatriciaTree
    ( Gr
    )
import qualified Data.Graph.Inductive.Query.DFS as Graph
import Data.List.Extra
    ( findIndex
    , groupSort
    )
import Data.List.NonEmpty
    ( NonEmpty (..)
    )
import Data.Map.Strict
    ( Map
    )
import qualified Data.Map.Strict as Map
import Data.Maybe
    ( fromJust
    )
import Data.Sequence
    ( Seq
    )
import Data.Set
    ( Set
    )
import qualified Data.Set as Set
import Data.Text
    ( Text
    , pack
    , unpack
    )
import GHC.Exts
    ( toList
    )
import Numeric.Natural
import System.IO
    ( Handle
    , IOMode (AppendMode)
    , hClose
    , openFile
    )

import Control.Monad.Reader
    ( MonadReader
    , asks
    )
import qualified Kore.Attribute.Axiom as Attribute
import qualified Kore.Attribute.Label as AttrLabel
import Kore.Internal.Condition
    ( Condition
    )
import Kore.Internal.Conditional
    ( Conditional (..)
    )
import Kore.Internal.Pattern
    ( toTermLike
    )
import Kore.Internal.Predicate as Predicate
import Kore.Internal.SideCondition
    ( SideCondition
    )
import Kore.Internal.TermLike
    ( Sort
    , TermLike
    )
import qualified Kore.Internal.TermLike as TermLike
import qualified Kore.Log as Log
import Kore.Repl.Data
import Kore.Step.RulePattern
    ( RewriteRule (..)
    , RulePattern (..)
    )
import Kore.Step.RulePattern as Rule
import Kore.Step.Simplification.Data
    ( MonadSimplify
    )
import qualified Kore.Step.Strategy as Strategy
import Kore.Strategies.Goal
import Kore.Strategies.ProofState
    ( ProofState (Goal)
    , ProofStateTransformer (ProofStateTransformer)
    , proofState
    )
import qualified Kore.Strategies.ProofState as ProofState.DoNotUse
import Kore.Strategies.Verification
import Kore.Syntax.Variable
    ( Variable
    )

-- | Creates a fresh execution graph for the given claim.
emptyExecutionGraph :: Claim claim => claim -> ExecutionGraph axiom
emptyExecutionGraph =
    Strategy.emptyExecutionGraph . extractConfig . RewriteRule . toRulePattern
  where
    extractConfig :: RewriteRule Variable -> CommonProofState
    extractConfig (RewriteRule RulePattern { left, requires }) =
        Goal $ Conditional left requires mempty

ruleReference
    :: (Either AxiomIndex ClaimIndex -> a)
    -> (RuleName -> a)
    -> RuleReference
    -> a
ruleReference f g ref =
    case ref of
        ByIndex axiomOrClaimIndex ->
            f axiomOrClaimIndex
        ByName ruleName ->
            g ruleName

-- | Get nth claim from the claims list.
getClaimByIndex
    :: MonadState (ReplState claim) m
    => Int
    -- ^ index in the claims list
    -> m (Maybe claim)
getClaimByIndex index = Lens.preuse $ field @"claims" . Lens.element index

-- | Get nth axiom from the axioms list.
getAxiomByIndex
    :: MonadState (ReplState claim) m
    => Int
    -- ^ index in the axioms list
    -> m (Maybe (Rule claim))
getAxiomByIndex index = Lens.preuse $ field @"axioms" . Lens.element index

-- | Get the leftmost axiom with a specific name from the axioms list.
getAxiomByName
    :: MonadState (ReplState claim) m
    => axiom ~ Rule claim
    => ToRulePattern axiom
    => String
    -- ^ label attribute
    -> m (Maybe axiom)
getAxiomByName name = do
    axioms <- Lens.use (field @"axioms")
    return $ find (isNameEqual name) axioms

-- | Get the leftmost claim with a specific name from the claim list.
getClaimByName
    :: MonadState (ReplState claim) m
    => Claim claim
    => String
    -- ^ label attribute
    -> m (Maybe claim)
getClaimByName name = do
    claims <- Lens.use (field @"claims")
    return $ find (isNameEqual name) claims

getClaimIndexByName
    :: MonadState (ReplState claim) m
    => Claim claim
    => String
    -- ^ label attribute
    -> m (Maybe ClaimIndex)
getClaimIndexByName name= do
    claims <- Lens.use (field @"claims")
    return $ ClaimIndex <$> findIndex (isNameEqual name) claims

getAxiomOrClaimByName
    :: MonadState (ReplState claim) m
    => Claim claim
    => axiom ~ Rule claim
    => RuleName
    -> m (Maybe (Either axiom claim))
getAxiomOrClaimByName (RuleName name) = do
    mAxiom <- getAxiomByName name
    case mAxiom of
        Nothing -> do
            mClaim <- getClaimByName name
            case mClaim of
                Nothing -> return Nothing
                Just claim ->
                    return . Just . Right $ claim
        Just axiom ->
            return . Just . Left $ axiom

isNameEqual
    :: ToRulePattern rule
    => String -> rule -> Bool
isNameEqual name rule =
    maybe
        False
        ( (== name) . unpack)
          (AttrLabel.unLabel
          . getNameText
          $ rule
        )

getNameText
    :: ToRulePattern rule
    => rule -> AttrLabel.Label
getNameText =
    Attribute.label
    . attributes
    . toRulePattern

-- | Transforms an axiom or claim index into an axiom or claim if they could be
-- found.
getAxiomOrClaimByIndex
    :: MonadState (ReplState claim) m
    => axiom ~ Rule claim
    => Either AxiomIndex ClaimIndex
    -> m (Maybe (Either axiom claim))
getAxiomOrClaimByIndex =
    fmap bisequence
        . bitraverse
            (getAxiomByIndex . coerce)
            (getClaimByIndex . coerce)

getInternalIdentifier
    :: ToRulePattern rule
    => rule -> Attribute.RuleIndex
getInternalIdentifier =
    Attribute.identifier
    . Rule.attributes
    . toRulePattern

-- | Update the currently selected claim to prove.
switchToProof :: MonadState (ReplState claim) m => claim -> ClaimIndex -> m ()
switchToProof claim cindex =
    modify (\st -> st
        { claim = claim
        , claimIndex = cindex
        , node = ReplNode 0
        })

-- | Get the internal representation of the execution graph.
getInnerGraph
    :: MonadState (ReplState claim) m
    => axiom ~ Rule claim
    => Claim claim
    => m (InnerGraph axiom)
getInnerGraph =
    fmap Strategy.graph getExecutionGraph

-- | Get the current execution graph
getExecutionGraph
    :: MonadState (ReplState claim) m
    => axiom ~ Rule claim
    => Claim claim
    => m (ExecutionGraph axiom)
getExecutionGraph = do
    ReplState { claimIndex, graphs, claim } <- get
    let mgraph = Map.lookup claimIndex graphs
    return $ fromMaybe (emptyExecutionGraph claim) mgraph

-- | Update the internal representation of the current execution graph.
updateInnerGraph
    :: forall claim axiom m
    .  MonadState (ReplState claim) m
    => axiom ~ Rule claim
    => InnerGraph axiom
    -> m ()
updateInnerGraph ig = do
    ReplState { claimIndex, graphs } <- get
    field @"graphs" Lens..=
        Map.adjust (updateInnerGraph0 ig) claimIndex graphs
  where
    updateInnerGraph0
        :: InnerGraph axiom
        -> ExecutionGraph axiom
        -> ExecutionGraph axiom
    updateInnerGraph0 graph Strategy.ExecutionGraph { root } =
        Strategy.ExecutionGraph { root, graph }

-- | Update the current execution graph.
updateExecutionGraph
    :: MonadState (ReplState claim) m
    => axiom ~ Rule claim
    => ExecutionGraph axiom
    -> m ()
updateExecutionGraph gph = do
    ReplState { claimIndex, graphs } <- get
    field @"graphs" Lens..= Map.insert claimIndex gph graphs

-- | Smoothes out nodes which have inDegree == outDegree == 1
-- (with the exception of the direct children of branching nodes).
-- This is done by computing the subgraph formed with only such nodes,
-- and replacing each component of the subgraph with one edge
-- in the original graph.
-- This assumes the execution graph is a directed tree
-- with its edges pointed "downwards" (from the root)
-- and is partially ordered (parent(node) < node).
smoothOutGraph :: Gr node edge -> Maybe (Gr node (Maybe edge))
smoothOutGraph graph = do
    let subGraph = Graph.nfilter inOutDegreeOne graph
    edgesToAdd <-
        traverse (componentToEdge subGraph) (Graph.components subGraph)
    let nodesToRemove = Graph.nodes subGraph
        liftedSubGraph = Graph.emap Just (Graph.delNodes nodesToRemove graph)
        liftedGraph = Graph.insEdges edgesToAdd liftedSubGraph
    return liftedGraph
  where
    inOutDegreeOne :: Graph.Node -> Bool
    inOutDegreeOne node =
        Graph.outdeg graph node == 1
        && Graph.indeg graph node == 1
        && not (all isBranchingNode $ Graph.pre graph node)
    componentToEdge
        :: Gr node edge
        -> [Graph.Node]
        -> Maybe (Graph.LEdge (Maybe edge))
    componentToEdge subGraph nodes =
        case filter (isTerminalNode subGraph) nodes of
            [node] -> makeNewEdge node node
            [node1, node2] ->
                if node1 < node2
                    then makeNewEdge node1 node2
                    else makeNewEdge node2 node1
            _ -> Nothing
    makeNewEdge
        :: Graph.Node
        -> Graph.Node
        -> Maybe (Graph.LEdge (Maybe edge))
    makeNewEdge node1 node2 = do
        nodePre <- headMay (Graph.pre graph node1)
        nodeSuc <- headMay (Graph.suc graph node2)
        return (nodePre, nodeSuc, Nothing)
    isBranchingNode :: Graph.Node -> Bool
    isBranchingNode node =
        Graph.outdeg graph node > 1
    isTerminalNode :: Gr node edge -> Graph.Node -> Bool
    isTerminalNode graph' node =
        Graph.indeg graph' node == 0 || Graph.outdeg graph' node == 0

-- | Get the node labels for the current claim.
getLabels
    :: MonadState (ReplState claim) m
    => m (Map String ReplNode)
getLabels = do
    ReplState { claimIndex, labels } <- get
    let mlabels = Map.lookup claimIndex labels
    return $ fromMaybe Map.empty mlabels

-- | Update the node labels for the current claim.
setLabels
    :: MonadState (ReplState claim) m
    => Map String ReplNode
    -> m ()
setLabels lbls = do
    ReplState { claimIndex, labels } <- get
    field @"labels" Lens..= Map.insert claimIndex lbls labels


-- | Get selected node (or current node for 'Nothing') and validate that it's
-- part of the execution graph.
getTargetNode
    :: MonadState (ReplState claim) m
    => Claim claim
    => Maybe ReplNode
    -- ^ node index
    -> m (Maybe ReplNode)
getTargetNode maybeNode = do
    currentNode <- Lens.use (field @"node")
    let node' = unReplNode $ fromMaybe currentNode maybeNode
    graph <- getInnerGraph
    if node' `elem` Graph.nodes graph
       then pure . Just . ReplNode $ node'
       else pure Nothing

-- | Get the configuration at selected node (or current node for 'Nothing').
getConfigAt
    :: MonadState (ReplState claim) m
    => Claim claim
    => Maybe ReplNode
    -> m (Maybe (ReplNode, CommonProofState))
getConfigAt maybeNode = do
    node' <- getTargetNode maybeNode
    case node' of
        Nothing -> pure Nothing
        Just n -> do
            graph' <- getInnerGraph
            pure $ Just (n, getLabel graph' (unReplNode n))
  where
    getLabel gr n = Graph.lab' . Graph.context gr $ n

-- | Get the rule used to reach selected node.
getRuleFor
    :: MonadState (ReplState claim) m
    => axiom ~ Rule claim
    => Claim claim
    => Maybe ReplNode
    -- ^ node index
    -> m (Maybe axiom)
getRuleFor maybeNode = do
    targetNode <- getTargetNode maybeNode
    graph' <- getInnerGraph
    pure $ fmap unReplNode targetNode >>= getRewriteRule . Graph.inn graph'
  where
    getRewriteRule
        :: [(a, b, Seq axiom)]
        -> Maybe axiom
    getRewriteRule = headMay . concatMap (toList . third)

    third :: forall a b c. (a, b, c) -> c
    third (_, _, c) = c

-- | Lifting function that takes logging into account.
liftSimplifierWithLogger
    :: forall a t m claim
    .  MonadState (ReplState claim) (t m)
    => MonadSimplify m
    => MonadIO m
    => Monad.Trans.MonadTrans t
    => MVar (Log.LogAction IO Log.SomeEntry)
    -> m a
    -> t m a
liftSimplifierWithLogger mLogger simplifier = do
    ReplState { koreLogOptions } <- get
    let Log.KoreLogOptions { logType, timestampsSwitch, exeName } = koreLogOptions
    (textLogger, maybeHandle) <- logTypeToLogger logType
    let logger =
            Log.koreLogFilters koreLogOptions
            $ Log.makeKoreLogger
                exeName
                timestampsSwitch
                textLogger
    _ <- Monad.Trans.lift . liftIO $ swapMVar mLogger logger
    result <- Monad.Trans.lift simplifier
    maybe (pure ()) (Monad.Trans.lift . liftIO . hClose) maybeHandle
    pure result
  where
    logTypeToLogger
        :: Log.KoreLogType
        -> t m (Log.LogAction IO Text, Maybe Handle)
    logTypeToLogger =
        \case
            Log.LogStdErr -> pure (Log.logTextStderr, Nothing)
            Log.LogFileText file -> do
                handle <- Monad.Trans.lift . liftIO $ openFile file AppendMode
                pure (Log.logTextHandle handle, Just handle)

-- | Run a single step for the data in state
-- (claim, axioms, claims, current node and execution graph).
runStepper
    :: MonadState (ReplState claim) (t m)
    => MonadReader (Config claim m) (t m)
    => Monad.Trans.MonadTrans t
    => MonadSimplify m
    => MonadIO m
    => Claim claim
    => t m StepResult
runStepper = do
    ReplState { claims, axioms, node } <- get
    (graph', res) <- runStepper' claims axioms node
    updateExecutionGraph graph'
    case res of
        SingleResult nextNode -> do
            field @"node" Lens..= nextNode
            pure res
        _                     -> pure res

-- | Run a single step for the current claim with the selected claims, axioms
-- starting at the selected node.
runStepper'
    :: MonadState (ReplState claim) (t m)
    => MonadReader (Config claim m) (t m)
    => axiom ~ Rule claim
    => Monad.Trans.MonadTrans t
    => MonadSimplify m
    => MonadIO m
    => Claim claim
    => [claim]
    -> [axiom]
    -> ReplNode
    -> t m (ExecutionGraph axiom, StepResult)
runStepper' claims axioms node = do
    ReplState { claim } <- get
    stepper <- asks stepper
    mvar <- asks logger
    gph <- getExecutionGraph
    gr@Strategy.ExecutionGraph { graph = innerGraph } <-
        liftSimplifierWithLogger mvar $ stepper claim claims axioms gph node
    pure . (,) gr
        $ case Graph.suc innerGraph (unReplNode node) of
            []       -> NoResult
            [single] ->
                case getNodeState innerGraph single of
                    Nothing -> NoResult
                    Just (StuckNode, _) -> NoResult
                    _ -> SingleResult . ReplNode $ single
            nodes -> BranchResult $ fmap ReplNode nodes

runUnifier
    :: MonadState (ReplState claim) (t m)
    => MonadReader (Config claim m) (t m)
    => Monad.Trans.MonadTrans t
    => MonadSimplify m
    => MonadIO m
    => SideCondition Variable
    -> TermLike Variable
    -> TermLike Variable
    -> t m (Either ReplOutput (NonEmpty (Condition Variable)))
runUnifier sideCondition first second = do
    unifier <- asks unifier
    mvar <- asks logger
    liftSimplifierWithLogger mvar
        . runUnifierWithExplanation
        $ unifier sideCondition first second

getNodeState :: InnerGraph axiom -> Graph.Node -> Maybe (NodeState, Graph.Node)
getNodeState graph node =
        fmap (\nodeState -> (nodeState, node))
        . proofState ProofStateTransformer
            { goalTransformer = const . Just $ UnevaluatedNode
            , goalRemainderTransformer = const . Just $ StuckNode
            , goalRewrittenTransformer = const . Just $ UnevaluatedNode
            , goalStuckTransformer = const . Just $ StuckNode
            , provenValue = Nothing
            }
        . Graph.lab'
        . Graph.context graph
        $ node

nodeToPattern
    :: InnerGraph axiom
    -> Graph.Node
    -> Maybe (Text, TermLike Variable)
nodeToPattern graph node =
    fmap (\x -> ("node" <> (pack . show) node, x))
    $ proofState ProofStateTransformer
    { goalTransformer = Just . toTermLike
    , goalRemainderTransformer = Just . toTermLike
    , goalRewrittenTransformer = Just . toTermLike
    , goalStuckTransformer = Just . toTermLike
    , provenValue = Nothing
    }
    . Graph.lab'
    . Graph.context graph
    $ node

-- | Adds or updates the provided alias.
addOrUpdateAlias
    :: forall m claim
    .  MonadState (ReplState claim) m
    => MonadError AliasError m
    => AliasDefinition
    -> m ()
addOrUpdateAlias alias@AliasDefinition { name, command } = do
    checkNameIsNotUsed
    checkCommandExists
    field @"aliases" Lens.%= Map.insert name alias
  where
    checkNameIsNotUsed :: m ()
    checkNameIsNotUsed =
        not . Set.member name <$> existingCommands
            >>= falseToError NameAlreadyDefined

    checkCommandExists :: m ()
    checkCommandExists = do
        cmds <- existingCommands
        let
            maybeCommand = headMay $ words command
            maybeExists = Set.member <$> maybeCommand <*> pure cmds
        maybe
            (Monad.Error.throwError UnknownCommand)
            (falseToError UnknownCommand)
            maybeExists

    existingCommands :: m (Set String)
    existingCommands =
        Set.union commandSet
        . Set.fromList
        . Map.keys
        <$> Lens.use (field @"aliases")

    falseToError :: AliasError -> Bool -> m ()
    falseToError e b =
        if b then pure () else Monad.Error.throwError e


findAlias
    :: MonadState (ReplState claim) m
    => String
    -> m (Maybe AliasDefinition)
findAlias name = Map.lookup name <$> Lens.use (field @"aliases")

substituteAlias
    :: AliasDefinition
    -> ReplAlias
    -> String
substituteAlias
    AliasDefinition { arguments, command }
    ReplAlias { arguments = actualArguments } =
    unwords
      . fmap replaceArguments
      . words
      $ command
  where
    values :: Map String AliasArgument
    values
      | length arguments == length actualArguments
        = Map.fromList $ zip arguments actualArguments
      | otherwise = Map.empty

    replaceArguments :: String -> String
    replaceArguments s = maybe s toString $ Map.lookup s values

    toString :: AliasArgument -> String
    toString = \case
        SimpleArgument str -> str
        QuotedArgument str -> "\"" <> str <> "\""

createClaim
    :: Claim claim
    => (claim, (Text, TermLike Variable))
    -> (AttrLabel.Label, claim)
createClaim (claim, (name, cpattern)) =
    ( AttrLabel.Label . Just
        $ maybe "" (<> "_") claimName <> name
    , fromRulePattern
        claim
        Rule.RulePattern
            { left = cpattern
            , antiLeft = Nothing
            , requires = Predicate.makeTruePredicate_
            , rhs = Rule.rhs . toRulePattern $ claim
            , attributes = Default.def
            }
    )
  where
    claimName = AttrLabel.unLabel . getNameText $ claim

conjOfClaims
    :: From claim (TermLike Variable)
    => [claim]
    -> Sort
    -> TermLike Variable
conjOfClaims claims sort =
    foldr
        TermLike.mkAnd
        (TermLike.mkTop sort)
        $ fmap from claims

generateInProgressClaims
    :: forall claim m axiom
    .  Claim claim
    => axiom ~ Rule claim
    => MonadState (ReplState claim) m
    => Maybe Natural
    -> m [(AttrLabel.Label, claim)]
generateInProgressClaims maybeNode = do
    currentNode <- defaultCurrentNode maybeNode
    graphs <- Lens.use (field @"graphs")
    claims <- Lens.use (field @"claims")
    currentClaimIndex <-
        Lens.use (field @"claimIndex")
    let claims' = removeAt currentClaimIndex claims
    let started =
            currentClaim currentNode
            : startedClaims graphs claims'
        notStarted = notStartedClaims graphs claims'
    return $ started <> notStarted
  where
    currentClaim = undefined
    defaultCurrentNode :: Maybe Natural -> m ReplNode
    defaultCurrentNode =
        maybe
            (Lens.use $ field @"node")
            (return . ReplNode . fromEnum)
    startedClaims
        :: Map.Map ClaimIndex (ExecutionGraph axiom)
        -> [claim]
        -> [(AttrLabel.Label, claim)]
    startedClaims graphs claims =
        fmap createClaim
        $ claimsWithPatterns graphs claims
        >>= sequence
    notStartedClaims
        :: Map.Map ClaimIndex (ExecutionGraph axiom)
        -> [claim]
        -> [(AttrLabel.Label, claim)]
    notStartedClaims graphs claims =
        let nsClaims =
                filter (not . isTrusted)
                    ( (claims !!)
                    . unClaimIndex
                    <$> Set.toList
                        (Set.difference
                            ( Set.fromList
                            $ fmap ClaimIndex [0..length claims - 1]
                            )
                            ( Set.fromList
                            $ Map.keys graphs
                            )
                        )
                    )
         in fmap (\c -> (getNameText c, c)) nsClaims
    removeAt
        :: ClaimIndex
        -> [claim]
        -> [claim]
    removeAt (ClaimIndex index) claims
        | index >= 0 && index < length claims =
            take index claims
            <> drop (index + 1) claims
        | otherwise = claims

claimsWithPatterns
    :: Map ClaimIndex (ExecutionGraph axiom)
    -> [claim]
    -> [(claim, [(Text, TermLike Variable)])]
claimsWithPatterns graphs claims =
    Bifunctor.bimap
        ((claims !!) . unClaimIndex)
        (findTerminalPatterns . Strategy.graph)
    <$> Map.toList graphs

findTerminalPatterns
    :: InnerGraph axiom
    -> [(Text, TermLike Variable)]
findTerminalPatterns graph =
    mapMaybe (nodeToPattern graph)
    . findLeafNodes
    $ graph

currentClaimSort
    :: Claim claim
    => MonadState (ReplState claim) m
    => m Sort
currentClaimSort = do
    claims <- Lens.use (field @"claim")
    return . TermLike.termLikeSort
        . Rule.onePathRuleToTerm
        . Rule.OnePathRule
        . toRulePattern
        $ claims

sortLeafsByType :: InnerGraph axiom -> Map.Map NodeState [Graph.Node]
sortLeafsByType graph =
    Map.fromList
        . groupSort
        . mapMaybe (getNodeState graph)
        . findLeafNodes
        $ graph


findLeafNodes :: InnerGraph axiom -> [Graph.Node]
findLeafNodes graph =
    filter ((==) 0 . Graph.outdeg graph) $ Graph.nodes graph

appReplOut :: ReplOut -> ReplOutput -> ReplOutput
appReplOut rout routput = routput <> ReplOutput [rout]

replOut
    :: (String -> a)
    -> (String -> a)
    -> ReplOut
    -> a
replOut f g =
    \case
        AuxOut str  -> f str
        KoreOut str -> g str

replOutputToString :: ReplOutput -> String
replOutputToString (ReplOutput out) =
    out >>= replOut id id

allProofs
    :: forall claim m
    .  Claim claim
    => MonadState (ReplState claim) m
    => m (Map.Map ClaimIndex GraphProofStatus)
allProofs = do
    graphs <- Lens.use (field @"graphs")
    claims <- Lens.use (field @"claims")
    let cindexes = ClaimIndex <$> [0..length claims - 1]
    return
        $ fmap inProgressProofs graphs
        `Map.union`
        notStartedProofs graphs (Map.fromList $ zip cindexes claims)

allProofsWithCurrentClaim
    :: forall claim m
    .  Claim claim
    => MonadState (ReplState claim) m
    => m (Map.Map ClaimIndex GraphProofStatus)
allProofsWithCurrentClaim = do
    graphs <- Lens.use (field @"graphs")
    claims <- Lens.use (field @"claims")
    currentClaimIndex <- Lens.use (field @"claimIndex")
    let cindexes = ClaimIndex <$> [0..length claims - 1]
    return
        $ currentProof currentClaimIndex graphs
        `Map.union`
        fmap inProgressProofs graphs
        `Map.union`
        notStartedProofs graphs (Map.fromList $ zip cindexes claims)

currentProof
    :: ClaimIndex
    -> Map.Map ClaimIndex (ExecutionGraph axiom)
    -> Map.Map ClaimIndex GraphProofStatus
currentProof index gphs =
    Map.singleton index
    . CurrentClaim
    . findLeafNodes
    . Strategy.graph
    . fromJust
    -- ^ should be safe since the any current ClaimIndex must
    -- have an ExecutionGraph in the state
    $ Map.lookup index gphs

inProgressProofs
    :: ExecutionGraph axiom
    -> GraphProofStatus
inProgressProofs =
    findProofStatus
    . sortLeafsByType
    . Strategy.graph

notStartedProofs
    :: forall claim axiom
    .  Claim claim
    => Map.Map ClaimIndex (ExecutionGraph axiom)
    -> Map.Map ClaimIndex claim
    -> Map.Map ClaimIndex GraphProofStatus
notStartedProofs gphs cls =
    notStartedOrTrusted <$> cls `Map.difference` gphs

notStartedOrTrusted
    :: forall claim
    .  Claim claim
    => claim
    -> GraphProofStatus
notStartedOrTrusted cl =
    if isTrusted cl
       then TrustedClaim
       else NotStarted

findProofStatus :: Map.Map NodeState [Graph.Node] -> GraphProofStatus
findProofStatus m =
    case Map.lookup StuckNode m of
        Nothing ->
            case Map.lookup UnevaluatedNode m of
                Nothing -> Completed
                Just ns -> InProgress ns
        Just ns -> StuckProof ns
