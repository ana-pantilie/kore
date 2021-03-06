module Test.Kore.Step.Simplification.And
    ( test_andSimplification
    ) where

import Prelude.Kore

import Test.Tasty

import qualified Kore.Internal.Condition as Condition
import qualified Kore.Internal.MultiAnd as MultiAnd
import qualified Kore.Internal.MultiOr as MultiOr
import Kore.Internal.OrPattern
    ( OrPattern
    )
import qualified Kore.Internal.OrPattern as OrPattern
import Kore.Internal.Pattern as Pattern
import Kore.Internal.Predicate
    ( makeAndPredicate
    , makeCeilPredicate
    , makeCeilPredicate_
    , makeEqualsPredicate
    , makeEqualsPredicate_
    , makeFalsePredicate_
    , makeTruePredicate
    , makeTruePredicate_
    )
import qualified Kore.Internal.SideCondition as SideCondition
    ( top
    )
import qualified Kore.Internal.Substitution as Substitution
import Kore.Internal.TermLike
import Kore.Step.Simplification.And
import qualified Kore.Step.Simplification.Not as Not

import Test.Kore.Step.MockSymbols
    ( testSort
    )
import qualified Test.Kore.Step.MockSymbols as Mock
import Test.Kore.Step.Simplification
import Test.Tasty.HUnit.Ext

test_andSimplification :: [TestTree]
test_andSimplification =
    [ testCase "And truth table" $ do
        assertEqual "false and false = false"
            OrPattern.bottom
            =<< evaluate (makeAnd [] [])
        assertEqual "false and true = false"
            OrPattern.bottom
            =<< evaluate (makeAnd [] [Pattern.top])
        assertEqual "true and false = false"
            OrPattern.bottom
            =<< evaluate (makeAnd [Pattern.top] [])
        assertEqual "true and true = true"
            OrPattern.top
            =<< evaluate (makeAnd [Pattern.top] [Pattern.top])

    , testCase "And with booleans" $ do
        assertEqual "false and something = false"
            OrPattern.bottom
            =<< evaluate (makeAnd [] [fOfXExpanded])
        assertEqual "something and false = false"
            OrPattern.bottom
            =<< evaluate (makeAnd [fOfXExpanded] [])
        assertEqual "true and something = something"
            (OrPattern.fromPatterns [fOfXExpanded])
            =<< evaluate (makeAnd [Pattern.top] [fOfXExpanded])
        assertEqual "something and true = something"
            (OrPattern.fromPatterns [fOfXExpanded])
            =<< evaluate (makeAnd [fOfXExpanded] [Pattern.top])

    , testCase "And with partial booleans" $ do
        assertEqual "false term and something = false"
            mempty
            =<< evaluatePatterns bottomTerm fOfXExpanded
        assertEqual "something and false term = false"
            mempty
            =<< evaluatePatterns fOfXExpanded bottomTerm
        assertEqual "false predicate and something = false"
            mempty
            =<< evaluatePatterns falsePredicate fOfXExpanded
        assertEqual "something and false predicate = false"
            mempty
            =<< evaluatePatterns fOfXExpanded falsePredicate

    , testGroup "And with normal patterns"
        [ testCase "And random terms" $ do
            let expect =
                    Conditional
                        { term = mkAnd plain0OfX plain1OfX
                        , predicate = makeTruePredicate Mock.testSort
                        , substitution = mempty
                        }
            actual <- evaluatePatterns plain0OfXExpanded plain1OfXExpanded
            assertEqual "" (OrPattern.fromPatterns [expect]) actual

        , testCase "And function terms" $ do
            let expect =
                    makeEqualsPredicate_ fOfX gOfX
                    & Condition.fromPredicate
                    & Pattern.withCondition fOfX
            actual <- evaluatePatterns fOfXExpanded gOfXExpanded
            assertEqual "" (OrPattern.fromPatterns [expect]) actual

        , testCase "And predicates" $ do
            let expect =
                    Conditional
                        { term = mkTop_
                        , predicate =
                            makeAndPredicate
                                (makeCeilPredicate_ fOfX)
                                (makeCeilPredicate_ gOfX)
                        , substitution = mempty
                        }
            actual <-
                evaluatePatterns
                    Conditional
                        { term = mkTop_
                        , predicate = makeCeilPredicate_ fOfX
                        , substitution = mempty
                        }
                    Conditional
                        { term = mkTop_
                        , predicate = makeCeilPredicate_ gOfX
                        , substitution = mempty
                        }
            assertEqual "" (OrPattern.fromPatterns [expect]) actual

        , testCase "And substitutions - simple" $ do
            let expect =
                    Conditional
                        { term = mkTop_
                        , predicate = makeTruePredicate_
                        , substitution = Substitution.unsafeWrap
                            [(inject Mock.y, fOfX), (inject Mock.z, gOfX)]
                        }
            actual <-
                evaluatePatterns
                    Conditional
                        { term = mkTop_
                        , predicate = makeTruePredicate_
                        , substitution =
                            Substitution.wrap
                            $ Substitution.mkUnwrappedSubstitution
                            [(inject Mock.y, fOfX)]
                        }
                    Conditional
                        { term = mkTop_
                        , predicate = makeTruePredicate_
                        , substitution =
                            Substitution.wrap
                            $ Substitution.mkUnwrappedSubstitution
                            [(inject Mock.z, gOfX)]
                        }
            assertEqual "" (OrPattern.fromPatterns [expect]) actual

        , testCase "And substitutions - multiple terms" $ do
            let
                expect =
                    Conditional
                        { term = mkAnd (mkAnd Mock.a Mock.b) Mock.c
                        , predicate = makeTruePredicate Mock.testSort
                        , substitution = mempty
                        }
            actual <- evaluatePatterns
                Conditional
                    { term = mkAnd Mock.a Mock.b
                    , predicate = makeTruePredicate_
                    , substitution = mempty
                    }
                Conditional
                    { term = mkAnd Mock.b Mock.c
                    , predicate = makeTruePredicate_
                    , substitution = mempty
                    }
            assertEqual "" (OrPattern.fromPatterns [expect]) actual

        , testCase "And substitutions - separate predicate" $ do
            let
                expect =
                    Conditional
                        { term = mkTop_
                        , predicate = makeEqualsPredicate_ fOfX gOfX
                        , substitution =
                            Substitution.unsafeWrap [(inject Mock.y, fOfX)]
                        }
            actual <- evaluatePatterns
                Conditional
                    { term = mkTop_
                    , predicate = makeTruePredicate_
                    , substitution =
                        Substitution.wrap
                        $ Substitution.mkUnwrappedSubstitution
                        [(inject Mock.y, fOfX)]
                    }
                Conditional
                    { term = mkTop_
                    , predicate = makeTruePredicate_
                    , substitution =
                        Substitution.wrap
                        $ Substitution.mkUnwrappedSubstitution
                        [(inject Mock.y, gOfX)]
                    }
            assertEqual "" (OrPattern.fromPatterns [expect]) actual

        , testCase "And substitutions - failure" $ do
            actual <-
                evaluatePatterns
                    Conditional
                        { term = mkTop_
                        , predicate = makeTruePredicate_
                        , substitution = Substitution.wrap
                            $ Substitution.mkUnwrappedSubstitution
                            [   ( inject Mock.y
                                , Mock.functionalConstr10 (mkElemVar Mock.x)
                                )
                            ]
                        }
                    Conditional
                        { term = mkTop_
                        , predicate = makeTruePredicate_
                        , substitution = Substitution.wrap
                            $ Substitution.mkUnwrappedSubstitution
                            [   ( inject Mock.y
                                , Mock.functionalConstr11 (mkElemVar Mock.x)
                                )
                            ]
                        }
            assertEqual "" OrPattern.bottom actual
            {-
            TODO(virgil): Uncomment this after substitution merge can handle
            function equality.

            assertEqual
                "Combines conditions with substitution merge condition"
                Pattern
                    { term = mkTop_
                    , predicate =
                        fst $ makeAndPredicate
                            (fst $ makeAndPredicate
                                (makeCeilPredicate_ fOfX)
                                (makeCeilPredicate_ gOfX)
                            )
                            (givemakeEqualsPredicate_ fOfX gOfX)
                    , substitution = [(y, fOfX)]
                    }
                (evaluatePatternsWithAttributes
                    [ (fSymbol, mock.functionAttributes)
                    , (gSymbol, mock.functionAttributes)
                    ]
                    Pattern
                        { term = mkTop_
                        , predicate = makeCeilPredicate_ fOfX
                        , substitution = [(y, fOfX)]
                        }
                    Pattern
                        { term = mkTop_
                        , predicate = makeCeilPredicate_ gOfX
                        , substitution = [(y, gOfX)]
                        }
                )
            -}
        ]
    , testGroup "Variable-function and"
        [ testCase "variable-term" $ do
            let expect =
                    Conditional
                        { term = fOfX
                        , predicate = makeTruePredicate Mock.testSort
                        , substitution = Substitution.unsafeWrap
                            [(inject Mock.y, fOfX)]
                        }
            actual <- evaluatePatterns yExpanded fOfXExpanded
            assertEqual "" (MultiOr.make [expect]) actual

        , testCase "term-variable" $ do
            let expect =
                    Conditional
                        { term = fOfX
                        , predicate = makeTruePredicate Mock.testSort
                        , substitution = Substitution.unsafeWrap
                            [(inject Mock.y, fOfX)]
                        }
            actual <- evaluatePatterns fOfXExpanded yExpanded
            assertEqual "" (MultiOr.make [expect]) actual
        ]

    , testGroup "constructor and"
        [ testCase "same constructors" $ do
            let expect =
                    Conditional
                        { term = Mock.constr10 fOfX
                        , predicate =
                            makeEqualsPredicate Mock.testSort fOfX gOfX
                        , substitution = mempty
                        }
            actual <-
                evaluatePatterns Conditional
                        { term = Mock.constr10 fOfX
                        , predicate = makeTruePredicate_
                        , substitution = mempty
                        }
                    Conditional
                        { term = Mock.constr10 gOfX
                        , predicate = makeTruePredicate_
                        , substitution = mempty
                        }
            assertEqual "" (MultiOr.make [expect]) actual

        , testCase "different constructors" $ do
            actual <-
                evaluatePatterns
                    Conditional
                        { term = Mock.constr10 fOfX
                        , predicate = makeTruePredicate_
                        , substitution = mempty
                        }
                    Conditional
                        { term = Mock.constr11 gOfX
                        , predicate = makeTruePredicate_
                        , substitution = mempty
                        }
            assertEqual "" (MultiOr.make []) actual
        ]

    -- (a or b) and (c or d) = (b and d) or (b and c) or (a and d) or (a and c)
    , testCase "And-Or distribution" $ do
        let expect =
                OrPattern.fromPatterns
                    [ makeEqualsPredicate_ fOfX gOfX
                        & Condition.fromPredicate
                        & Pattern.withCondition fOfX
                    , Conditional
                        { term = fOfX
                        , predicate = makeCeilPredicate Mock.testSort gOfX
                        , substitution = mempty
                        }
                    , Conditional
                        { term = gOfX
                        , predicate = makeCeilPredicate Mock.testSort fOfX
                        , substitution = mempty
                        }
                    , Conditional
                        { term = mkTop_
                        , predicate =
                            makeAndPredicate
                                (makeCeilPredicate_ fOfX)
                                (makeCeilPredicate_ gOfX)
                        , substitution = mempty
                        }
                    ]
        actual <-
            evaluate
                (makeAnd
                    [ fOfXExpanded
                    , Conditional
                        { term = mkTop_
                        , predicate = makeCeilPredicate_ fOfX
                        , substitution = mempty
                        }
                    ]
                    [ gOfXExpanded
                    , Conditional
                        { term = mkTop_
                        , predicate = makeCeilPredicate_ gOfX
                        , substitution = mempty
                        }
                    ]
                )
        assertEqual "Distributes or" expect actual
    , testCase "Predicates are not duplicated" $ do
        let expect =
                Conditional
                    { term = Mock.constr10 fOfX
                    , predicate =
                        makeAndPredicate
                            (makeCeilPredicate Mock.testSort fOfX)
                            (makeEqualsPredicate_ fOfX gOfX)
                    , substitution = mempty
                    }
        actual <-
            evaluatePatterns
                Conditional
                    { term = Mock.constr10 fOfX
                    , predicate = makeCeilPredicate_ fOfX
                    , substitution = mempty
                    }
                Conditional
                    { term = Mock.constr10 gOfX
                    , predicate = makeCeilPredicate_ fOfX
                    , substitution = mempty
                    }
        assertEqual "" (MultiOr.make [expect]) actual
    ]
  where
    yExpanded = Conditional
        { term = mkElemVar Mock.y
        , predicate = makeTruePredicate_
        , substitution = mempty
        }
    fOfX = Mock.f (mkElemVar Mock.x)
    fOfXExpanded = Pattern.fromTermLike fOfX
    gOfX = Mock.g (mkElemVar Mock.x)
    gOfXExpanded = Conditional
        { term = gOfX
        , predicate = makeTruePredicate_
        , substitution = mempty
        }
    plain0OfX = Mock.plain10 (mkElemVar Mock.x)
    plain0OfXExpanded = Conditional
        { term = plain0OfX
        , predicate = makeTruePredicate_
        , substitution = mempty
        }
    plain1OfX = Mock.plain11 (mkElemVar Mock.x)
    plain1OfXExpanded = Conditional
        { term = plain1OfX
        , predicate = makeTruePredicate_
        , substitution = mempty
        }
    bottomTerm = Conditional
        { term = mkBottom_
        , predicate = makeTruePredicate_
        , substitution = mempty
        }
    falsePredicate = Conditional
        { term = mkTop_
        , predicate = makeFalsePredicate_
        , substitution = mempty
        }

makeAnd
    :: [Pattern VariableName]
    -> [Pattern VariableName]
    -> And Sort (OrPattern VariableName)
makeAnd first second =
    And
        { andSort = findSort (first ++ second)
        , andFirst = OrPattern.fromPatterns first
        , andSecond = OrPattern.fromPatterns second
        }

findSort :: [Pattern VariableName] -> Sort
findSort [] = testSort
findSort ( Conditional {term} : _ ) = termLikeSort term

evaluate :: And Sort (OrPattern VariableName) -> IO (OrPattern VariableName)
evaluate And { andFirst, andSecond } =
    MultiAnd.make [andFirst, andSecond]
    & simplify Not.notSimplifier SideCondition.top
    & runSimplifier Mock.env

evaluatePatterns
    :: Pattern VariableName
    -> Pattern VariableName
    -> IO (OrPattern VariableName)
evaluatePatterns first second =
    MultiAnd.make [first, second]
    & makeEvaluate Not.notSimplifier SideCondition.top
    & runSimplifierBranch Mock.env
    & fmap OrPattern.fromPatterns
