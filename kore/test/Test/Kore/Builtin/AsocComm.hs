module Test.Kore.Builtin.AsocComm where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Kore.Builtin.Builtin as Builtin
import Kore.Internal.TermLike
    ( TermLike
    )
import Kore.Syntax.Variable
import qualified Test.Kore.Step.MockSymbols as Mock

test_toKey :: [TestTree]
test_toKey =
    [ testCase "toKey func" $ do
        let val = Mock.f Mock.a :: TermLike Variable
            key = Builtin.toKey val
        assertEqual "not key" key Nothing
    ]
