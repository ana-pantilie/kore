{-|
Module      : Kore.ASTVerifier.ModuleVerifier
Description : Tools for verifying the wellformedness of a Kore 'Module'.
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : POSIX
-}
module Kore.ASTVerifier.ModuleVerifier
    ( verifyModule
    , verifyUniqueNames
    ) where

import qualified Data.Map as Map
import           Data.Text
                 ( Text )

import           Kore.ASTVerifier.AttributesVerifier
import           Kore.ASTVerifier.Error
import qualified Kore.ASTVerifier.SentenceVerifier as SentenceVerifier
import qualified Kore.Attribute.Symbol as Attribute
import qualified Kore.Builtin as Builtin
import           Kore.Error
import           Kore.IndexedModule.IndexedModule
import           Kore.Syntax
import           Kore.Syntax.Definition
import           Kore.Unparser
import qualified Kore.Verified as Verified

{-|'verifyUniqueNames' verifies that names defined in a module are unique both
within the module and outside, using the provided name set. -}
verifyUniqueNames
    :: Unparse pat
    => Map.Map Text AstLocation
    -- ^ Names that are already defined.
    -> Module (Sentence pat)
    -> Either (Error VerifyError) (Map.Map Text AstLocation)
    -- ^ On success returns the names that were previously defined together with
    -- the names defined in the given 'Module'.
verifyUniqueNames existingNames koreModule =
    withContext
        ("module '" ++ getModuleNameForError (moduleName koreModule) ++ "'")
        (SentenceVerifier.verifyUniqueNames
            (moduleSentences koreModule)
            existingNames)

{-|'verifyModule' verifies the welformedness of a Kore 'Module'. -}
verifyModule
    :: AttributesVerification Attribute.Symbol axiomAtts
    -> Builtin.Verifiers
    -> IndexedModule ParsedPattern Attribute.Symbol axiomAtts
    -> Either (Error VerifyError) (Module Verified.Sentence)
verifyModule attributesVerification builtinVerifiers indexedModule =
    withContext
        (  "module '"
        ++ getModuleNameForError (indexedModuleName indexedModule)
        ++ "'"
        )
        (do
            verifyAttributes
                (snd (indexedModuleAttributes indexedModule))
                attributesVerification
            let rawSorts =
                    snd
                    <$> indexedModuleSortDescriptions indexedModule
                rawSymbols =
                    snd
                    <$> indexedModuleSymbolSentences indexedModule
                rawAliases =
                    snd
                    <$> indexedModuleAliasSentences indexedModule
                rawClaims =
                    snd
                    <$> indexedModuleClaims indexedModule
                rawAxioms =
                    snd
                    <$> indexedModuleAxioms indexedModule
            sortIndex <- SentenceVerifier.verifySorts rawSorts
            symbolIndex <-
                SentenceVerifier.verifySymbols
                    indexedModule
                    sortIndex
                    rawSymbols
            aliasIndex <-
                SentenceVerifier.verifyAliases
                    symbolIndex
                    builtinVerifiers
                    indexedModule
                    rawAliases
            ruleIndex <-
                SentenceVerifier.verifyRules
                    aliasIndex
                    builtinVerifiers
                    indexedModule
                    rawClaims
                    rawAxioms
            let moduleSentences = SentenceVerifier.getVerifiedSentences ruleIndex
            return Module { moduleName, moduleSentences, moduleAttributes }
        )
  where
    moduleName = indexedModuleName indexedModule
    (_, moduleAttributes) = indexedModuleAttributes indexedModule
