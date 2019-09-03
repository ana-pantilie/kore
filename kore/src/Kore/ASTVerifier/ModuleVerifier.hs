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

import           Control.Applicative
                 ( Alternative (..) )
import           Control.Lens
                 ( (%=) )
import qualified Control.Lens as Lens
import qualified Control.Monad.Reader.Class as Reader
import qualified Control.Monad.State.Class as State
import qualified Control.Monad.Trans as Trans
import qualified Data.Foldable as Foldable
import           Data.Function
import           Data.Generics.Product
import qualified Data.List as List
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Text
                 ( Text )

import           Kore.AST.Error
import           Kore.ASTVerifier.AliasVerifier
import           Kore.ASTVerifier.AttributesVerifier
import           Kore.ASTVerifier.Error
import           Kore.ASTVerifier.SentenceVerifier
                 ( SentenceVerifier, verifyHookedSorts, verifyHookedSymbols,
                 verifySorts )
import qualified Kore.ASTVerifier.SentenceVerifier as SentenceVerifier
import           Kore.ASTVerifier.Verifier
import           Kore.Attribute.Parser
                 ( ParseAttributes )
import qualified Kore.Attribute.Parser as Attribute.Parser
import           Kore.Error
import           Kore.IndexedModule.IndexedModule as IndexedModule
import           Kore.Syntax
import           Kore.Syntax.Definition
import           Kore.Unparser

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
            existingNames
        )

verifyModule :: ModuleName -> Verifier VerifiedModule'
verifyModule name = lookupVerifiedModule name >>= maybe notCached cached
  where
    cached = return
    notCached = verifyUncachedModule name

verifyUncachedModule :: ModuleName -> Verifier VerifiedModule'
verifyUncachedModule name = whileImporting name $ do
    module' <- lookupParsedModule name
    let Module { moduleSentences } = module'
        sentences = List.sort moduleSentences
    (_, indexedModule) <-
            withModuleContext name (newVerifiedModule module')
        >>= SentenceVerifier.runSentenceVerifier
            (do
                verifyImports sentences
                withModuleContext name $ do
                    -- TODO: The corresponding functions in
                    -- Kore.IndexedModule.IndexedModule can go away.
                    verifySorts         sentences
                    verifySymbols       sentences
                    verifyHookedSorts   sentences
                    verifyHookedSymbols sentences
                    verifyNonHooks      sentences
                    verifyAliases       sentences
                    verifyAxioms        sentences
                    verifyClaims        sentences
            )
    _ <-
        withModuleContext name
        $ internalIndexedModuleSubsorts indexedModule
    field @"verifiedModules" %= Map.insert name indexedModule
    return indexedModule

newVerifiedModule :: Module ParsedSentence -> Verifier VerifiedModule'
newVerifiedModule module' = do
    VerifierContext { implicitModule } <- Reader.ask
    let Module { moduleName, moduleAttributes } = module'
    attrs <- parseAttributes' moduleAttributes
    return
        ( indexedModuleWithDefaultImports moduleName implicitModule
        & Lens.set (field @"indexedModuleAttributes") (attrs, moduleAttributes)
        )

verifyImports :: [ParsedSentence] -> SentenceVerifier ()
verifyImports = Foldable.traverse_ verifyImport . mapMaybe projectSentenceImport

verifyImport :: SentenceImport ParsedPattern -> SentenceVerifier ()
verifyImport sentence =
    withSentenceImportContext sentence $ do
        let SentenceImport { sentenceImportAttributes = attrs0 } = sentence
        attrs1 <- parseAttributes' attrs0
        let importName = sentenceImportModuleName sentence
        verified <- Trans.lift $ verifyModule importName
        State.modify' $ addImport verified attrs1 attrs0
  where
    addImport verified attrs1 attrs0 =
        Lens.over
            (field @"indexedModuleImports")
            ((attrs1, attrs0, verified) :)

parseAttributes'
    :: forall attrs error e
    .  (MonadError (Error e) error, ParseAttributes attrs)
    => Attributes
    -> error attrs
parseAttributes' =
    Attribute.Parser.liftParser . Attribute.Parser.parseAttributes

verifySymbols :: [ParsedSentence] -> SentenceVerifier ()
verifySymbols = Foldable.traverse_ verifySymbol . mapMaybe project
  where
    project sentence =
        projectSentenceSymbol sentence <|> projectSentenceHookedSymbol sentence

verifySymbol :: SentenceSymbol ParsedPattern -> SentenceVerifier ()
verifySymbol sentence =
    withSentenceSymbolContext sentence $ do
        verified <- SentenceVerifier.verifySymbolSentence sentence
        attrs <- parseAttributes' $ sentenceSymbolAttributes sentence
        State.modify' $ addSymbol verified attrs
  where
    addSymbol verified attrs =
        Lens.over
            (field @"indexedModuleSymbolSentences")
            (Map.insert name (attrs, verified))
      where
        Symbol { symbolConstructor = name } = sentenceSymbolSymbol verified

verifyAxioms :: [ParsedSentence] -> SentenceVerifier ()
verifyAxioms = Foldable.traverse_ verifyAxiom . mapMaybe projectSentenceAxiom

verifyAxiom :: SentenceAxiom ParsedPattern -> SentenceVerifier ()
verifyAxiom sentence =
    withSentenceAxiomContext sentence $ do
        verified <- SentenceVerifier.verifyAxiomSentence sentence
        attrs <- parseAttributes' $ sentenceAxiomAttributes sentence
        State.modify $ addAxiom verified attrs
  where
    addAxiom verified attrs =
        Lens.over
            (field @"indexedModuleAxioms")
            ((attrs, verified) :)

verifyClaims
    :: [ParsedSentence]
    -> SentenceVerifier ()
verifyClaims = Foldable.traverse_ verifyClaim . mapMaybe projectSentenceClaim

verifyClaim :: SentenceClaim ParsedPattern -> SentenceVerifier ()
verifyClaim sentence =
    withSentenceClaimContext sentence $ do
        verified <- SentenceVerifier.verifyClaimSentence sentence
        attrs <- parseAttributes' $ sentenceClaimAttributes sentence
        State.modify' $ addClaim verified attrs
  where
    addClaim verified attrs =
        Lens.over
            (field @"indexedModuleClaims")
            ((attrs, verified) :)

verifyNonHooks
    :: [ParsedSentence]
    -> SentenceVerifier ()
verifyNonHooks sentences=
    Foldable.traverse_ verifyNonHook nonHookSentences
  where
    nonHookSentences = mapMaybe project sentences
    project (SentenceHookSentence _) = Nothing
    project sentence = Just sentence

verifyNonHook :: ParsedSentence -> SentenceVerifier ()
verifyNonHook sentence =
    withSentenceContext sentence $ do
        VerifierContext { attributesVerification } <- Reader.ask
        verifyNoHookAttribute attributesVerification
            $ sentenceAttributes sentence
