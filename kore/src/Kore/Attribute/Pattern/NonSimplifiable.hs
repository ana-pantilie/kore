{- |
Copyright   : (c) Runtime Verification, 2019
License     : NCSA

 -}

module Kore.Attribute.Pattern.NonSimplifiable
    ( NonSimplifiable (..)
    ) where

import Control.DeepSeq
import Data.Hashable
import Data.Monoid
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC

import Kore.Attribute.Synthetic
import Kore.Debug
import Kore.Domain.Builtin
import Kore.Internal.Alias
    ( Alias
    )
import Kore.Internal.Symbol
    ( Symbol
    )
import qualified Kore.Internal.Symbol as Symbol
import Kore.Syntax
import Kore.Syntax.Application
    ( Application (..)
    )

newtype NonSimplifiable =
    NonSimplifiable
        { isNonSimplifiable :: Maybe NonSimplifiableHead
        }
    deriving (Eq, GHC.Generic, Ord, Show)

instance SOP.Generic NonSimplifiable

instance SOP.HasDatatypeInfo NonSimplifiable

instance Debug NonSimplifiable

instance Diff NonSimplifiable where
    diffPrec = diffPrecIgnore

instance NFData NonSimplifiable

instance Hashable NonSimplifiable

instance Synthetic NonSimplifiable (And sort) where
    synthetic = const (NonSimplifiable Nothing)
    {-# INLINE synthetic #-}

instance Synthetic NonSimplifiable (Bottom sort) where
    synthetic = const (NonSimplifiable Nothing)
    {-# INLINE synthetic #-}

instance Synthetic NonSimplifiable (Application Symbol) where
    synthetic application
        | Symbol.isConstructor symbol
        , childrenAreNonSimplifiable children =
            NonSimplifiable . Just $ ConstructorHead

        | Symbol.isSortInjection symbol
        , childrenAreNonSimplifiable children
        , childrenAreNotSortInjections children =
            NonSimplifiable . Just $ SortInjectionHead

        | otherwise =
            NonSimplifiable Nothing
      where
        symbol = applicationSymbolOrAlias application
        children = applicationChildren application
        childrenAreNonSimplifiable xs =
            elem (NonSimplifiable Nothing) xs
        childrenAreNotSortInjections xs =
            not $ elem (NonSimplifiable . Just $ SortInjectionHead) xs

instance Synthetic NonSimplifiable (Application (Alias patternType)) where
    synthetic = const (NonSimplifiable Nothing)
    {-# INLINE synthetic #-}

instance Synthetic NonSimplifiable (Ceil sort) where
    synthetic = const (NonSimplifiable Nothing)
    {-# INLINE synthetic #-}

instance Synthetic NonSimplifiable (DomainValue sort) where
    synthetic = const (NonSimplifiable Nothing)
    {-# INLINE synthetic #-}

instance Synthetic NonSimplifiable (Equals sort) where
    synthetic = const (NonSimplifiable Nothing)
    {-# INLINE synthetic #-}

instance Synthetic NonSimplifiable (Exists sort variable) where
    synthetic = const (NonSimplifiable Nothing)

instance Synthetic NonSimplifiable (Floor sort) where
    synthetic = const (NonSimplifiable Nothing)
    {-# INLINE synthetic #-}

instance Synthetic NonSimplifiable (Forall sort variable) where
    synthetic = const (NonSimplifiable Nothing)
    {-# INLINE synthetic #-}

instance Synthetic NonSimplifiable (Iff sort) where
    synthetic = const (NonSimplifiable Nothing)
    {-# INLINE synthetic #-}

instance Synthetic NonSimplifiable (Implies sort) where
    synthetic = const (NonSimplifiable Nothing)
    {-# INLINE synthetic #-}

instance Synthetic NonSimplifiable (In sort) where
    synthetic = const (NonSimplifiable Nothing)
    {-# INLINE synthetic #-}

instance Synthetic NonSimplifiable (Mu sort) where
    synthetic = const (NonSimplifiable Nothing)
    {-# INLINE synthetic #-}

instance Synthetic NonSimplifiable (Next sort) where
    synthetic = const (NonSimplifiable Nothing)
    {-# INLINE synthetic #-}

instance Synthetic NonSimplifiable (Not sort) where
    synthetic = const (NonSimplifiable Nothing)
    {-# INLINE synthetic #-}

instance Synthetic NonSimplifiable (Nu sort) where
    synthetic = const (NonSimplifiable Nothing)
    {-# INLINE synthetic #-}

instance Synthetic NonSimplifiable (Or sort) where
    synthetic = const (NonSimplifiable Nothing)
    {-# INLINE synthetic #-}

instance Synthetic NonSimplifiable (Rewrites sort) where
    synthetic = const (NonSimplifiable Nothing)
    {-# INLINE synthetic #-}

instance Synthetic NonSimplifiable (Builtin key) where
    synthetic =
        \case
            BuiltinInt _    -> NonSimplifiable . Just $ BuiltinHead
            BuiltinBool _   -> NonSimplifiable . Just $ BuiltinHead
            BuiltinString _ -> NonSimplifiable . Just $ BuiltinHead
            _               -> NonSimplifiable Nothing
    {-# INLINE synthetic #-}

instance Synthetic NonSimplifiable Inhabitant where
    synthetic = const (NonSimplifiable Nothing)
    {-# INLINE synthetic #-}

data NonSimplifiableHead = ConstructorHead
                         | SortInjectionHead
                         | BuiltinHead
    deriving (Eq, GHC.Generic, Ord, Show)

instance SOP.Generic NonSimplifiableHead

instance SOP.HasDatatypeInfo NonSimplifiableHead

instance Debug NonSimplifiableHead

instance Diff NonSimplifiableHead where
    diffPrec = diffPrecIgnore

instance NFData NonSimplifiableHead

instance Hashable NonSimplifiableHead
