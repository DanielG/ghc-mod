{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{- | "GHC.Syb.Utils" provides common utilities for the Ghc Api,
     either based on Data\/Typeable or for use with Data.Generics
     over Ghc Api types.

example output of 'showData' on 'parsedSource', 'renamedSource', and
'typecheckedSource' for a trivial @HelloWorld@ module, compared with
'ppr' output:

@
------------------------- pretty-printed parsedSource
module HelloWorld where
main = putStrLn "Hello, World!"
------------------------- pretty-printed renamedSource
Just (HelloWorld.main = System.IO.putStrLn "Hello, World!",
      [import Prelude],
      Nothing,
      Nothing,

(HaddockModInfo
 (Nothing)
 (Nothing)
 (Nothing)
 (Nothing)))
------------------------- pretty-printed typecheckedSource
Just <AbsBinds [] [] [HelloWorld.main <= [] main]
        HelloWorld.main :: GHC.IOBase.IO ()
        []
        { main = System.IO.putStrLn "Hello, World!" }>
------------------------- shown parsedSource

(L {HelloWorld.hs:1:0}
 (HsModule
  (Just
   (L {HelloWorld.hs:1:7-16} {ModuleName: HelloWorld}))
  (Nothing)
  []
  [
   (L {HelloWorld.hs:2:0-30}
    (ValD
     (FunBind
      (L {HelloWorld.hs:2:0-3}
       (Unqual {OccName: main}))
      (False)
      (MatchGroup
       [
        (L {HelloWorld.hs:2:0-30}
         (Match
          []
          (Nothing)
          (GRHSs
           [
            (L {HelloWorld.hs:2:7-30}
             (GRHS
              []
              (L {HelloWorld.hs:2:7-30}
               (HsApp
                (L {HelloWorld.hs:2:7-14}
                 (HsVar
                  (Unqual {OccName: putStrLn})))
                (L {HelloWorld.hs:2:16-30}
                 (HsLit
                  (HsString {FastString: "Hello, World!"})))))))]
           (EmptyLocalBinds))))] {!type placeholder here?!})
      (WpHole) {!NameSet placeholder here!}
      (Nothing))))]
  (Nothing)
  (HaddockModInfo
   (Nothing)
   (Nothing)
   (Nothing)
   (Nothing))
  (Nothing)))
------------------------- shown renamedSource

((,,,,)
 (HsGroup
  (ValBindsOut
   [
    ((,)
     (NonRecursive) {Bag(Located (HsBind Name)):
     [
      (L {HelloWorld.hs:2:0-30}
       (FunBind
        (L {HelloWorld.hs:2:0-3} {Name: HelloWorld.main})
        (False)
        (MatchGroup
         [
          (L {HelloWorld.hs:2:0-30}
           (Match
            []
            (Nothing)
            (GRHSs
             [
              (L {HelloWorld.hs:2:7-30}
               (GRHS
                []
                (L {HelloWorld.hs:2:7-30}
                 (HsApp
                  (L {HelloWorld.hs:2:7-14}
                   (HsVar {Name: System.IO.putStrLn}))
                  (L {HelloWorld.hs:2:16-30}
                   (HsLit
                    (HsString {FastString: "Hello, World!"})))))))]
             (EmptyLocalBinds))))] {!type placeholder here?!})
        (WpHole) {NameSet:
        [{Name: System.IO.putStrLn}]}
        (Nothing)))]})]
   [])
  []
  []
  []
  []
  []
  []
  []
  []
  [])
 [
  (L {Implicit import declaration}
   (ImportDecl
    (L {Implicit import declaration} {ModuleName: Prelude})
    (False)
    (False)
    (Nothing)
    (Nothing)))]
 (Nothing)
 (Nothing)
 (HaddockModInfo
  (Nothing)
  (Nothing)
  (Nothing)
  (Nothing)))
------------------------- shown typecheckedSource
{Bag(Located (HsBind Var)):
[
 (L {HelloWorld.hs:2:0-30}
  (AbsBinds
   []
   []
   [
    ((,,,)
     [] {Var: HelloWorld.main} {Var: main}
     [])] {Bag(Located (HsBind Var)):
   [
    (L {HelloWorld.hs:2:0-30}
     (FunBind
      (L {HelloWorld.hs:2:0-3} {Var: main})
      (False)
      (MatchGroup
       [
        (L {HelloWorld.hs:2:0-30}
         (Match
          []
          (Nothing)
          (GRHSs
           [
            (L {HelloWorld.hs:2:7-30}
             (GRHS
              []
              (L {HelloWorld.hs:2:7-30}
               (HsApp
                (L {HelloWorld.hs:2:7-14}
                 (HsVar {Var: System.IO.putStrLn}))
                (L {HelloWorld.hs:2:16-30}
                 (HsLit
                  (HsString {FastString: "Hello, World!"})))))))]
           (EmptyLocalBinds))))] GHC.IOBase.IO ())
      (WpHole) {!NameSet placeholder here!}
      (Nothing)))]}))]}
@
-}
module GHC.SYB.Utils where

import Data.Generics

-- import qualified GHC.Paths
import PprTyThing()
import DynFlags
import GHC hiding (moduleName)
import Outputable hiding (space)
import SrcLoc()
import qualified OccName(occNameString)
import Bag(Bag,bagToList)
import Var(Var)
import FastString(FastString)
#if __GLASGOW_HASKELL__ >= 800
import NameSet(NameSet,nameSetElemsStable)
#else
#if __GLASGOW_HASKELL__ >= 709
import NameSet(NameSet,nameSetElems)
#else
import NameSet(NameSet,nameSetToList)
#endif
#endif

#if __GLASGOW_HASKELL__ < 700
import GHC.SYB.Instances
#endif

import Control.Monad
import Data.List

#if __GLASGOW_HASKELL__ >= 800
nameSetElems :: NameSet -> [Name]
nameSetElems = nameSetElemsStable
#endif

#if __GLASGOW_HASKELL__ < 709
nameSetElems :: NameSet -> [Name]
nameSetElems = nameSetToList
#endif

showSDoc_ :: SDoc -> String
#if __GLASGOW_HASKELL__ >= 707
showSDoc_ = showSDoc unsafeGlobalDynFlags
#elif __GLASGOW_HASKELL__ < 706
showSDoc_ = showSDoc
#else
showSDoc_ = showSDoc tracingDynFlags
#endif

-- | Ghc Ast types tend to have undefined holes, to be filled
--   by later compiler phases. We tag Asts with their source,
--   so that we can avoid such holes based on who generated the Asts.
data Stage = Parser | Renamer | TypeChecker deriving (Eq,Ord,Show)

-- | Generic Data-based show, with special cases for GHC Ast types,
--   and simplistic indentation-based layout (the 'Int' parameter); 
--   showing abstract types abstractly and avoiding known potholes 
--   (based on the 'Stage' that generated the Ast)
showData :: Data a => Stage -> Int -> a -> String
showData stage n = 
  generic `ext1Q` list `extQ` string `extQ` fastString `extQ` srcSpan 
          `extQ` name `extQ` occName `extQ` moduleName `extQ` var `extQ` dataCon
          `extQ` overLit
          `extQ` bagName `extQ` bagRdrName `extQ` bagVar `extQ` nameSet
#if __GLASGOW_HASKELL__ <= 708
          `extQ` postTcType
#endif
          `extQ` fixity
  where generic :: Data a => a -> String
        generic t = indent n ++ "(" ++ showConstr (toConstr t)
                 ++ space (concat (intersperse " " (gmapQ (showData stage (n+1)) t))) ++ ")"
        space "" = ""
        space s  = ' ':s
        indent i = "\n" ++ replicate i ' '
        string     = show :: String -> String
        fastString = ("{FastString: "++) . (++"}") . show :: FastString -> String
        list l     = indent n ++ "[" 
                              ++ concat (intersperse "," (map (showData stage (n+1)) l)) ++ "]"

        name       = ("{Name: "++) . (++"}") . showSDoc_ . ppr :: Name -> String
        occName    = ("{OccName: "++) . (++"}") .  OccName.occNameString 
        moduleName = ("{ModuleName: "++) . (++"}") . showSDoc_ . ppr :: ModuleName -> String
        srcSpan    = ("{"++) . (++"}") . showSDoc_ . ppr :: SrcSpan -> String
        var        = ("{Var: "++) . (++"}") . showSDoc_ . ppr :: Var -> String
        dataCon    = ("{DataCon: "++) . (++"}") . showSDoc_ . ppr :: DataCon -> String

        overLit :: (HsOverLit RdrName) -> String
        overLit    = ("{HsOverLit:"++) . (++"}") . showSDoc_ . ppr

        bagRdrName:: Bag (Located (HsBind RdrName)) -> String
        bagRdrName = ("{Bag(Located (HsBind RdrName)): "++) . (++"}") . list . bagToList 
        bagName   :: Bag (Located (HsBind Name)) -> String
        bagName    = ("{Bag(Located (HsBind Name)): "++) . (++"}") . list . bagToList 
        bagVar    :: Bag (Located (HsBind Var)) -> String
        bagVar     = ("{Bag(Located (HsBind Var)): "++) . (++"}") . list . bagToList 

        nameSet | stage `elem` [Parser,TypeChecker] 
                = const ("{!NameSet placeholder here!}") :: NameSet -> String
                | otherwise     
                = ("{NameSet: "++) . (++"}") . list . nameSetElems 

#if __GLASGOW_HASKELL__ <= 708
        postTcType | stage<TypeChecker = const "{!type placeholder here?!}" :: PostTcType -> String
                   | otherwise     = showSDoc_ . ppr :: Type -> String
#endif
        fixity | stage<Renamer = const "{!fixity placeholder here?!}" :: GHC.Fixity -> String
               | otherwise     = ("{Fixity: "++) . (++"}") . showSDoc_ . ppr :: GHC.Fixity -> String


-- | Like 'everything', but avoid known potholes, based on the 'Stage' that
--   generated the Ast.
everythingStaged :: Stage -> (r -> r -> r) -> r -> GenericQ r -> GenericQ r
everythingStaged stage k z f x 
  | (const False
#if __GLASGOW_HASKELL__ <= 708
      `extQ` postTcType
#endif
      `extQ` fixity `extQ` nameSet) x = z
  | otherwise = foldl k (f x) (gmapQ (everythingStaged stage k z f) x)
  where nameSet    = const (stage `elem` [Parser,TypeChecker]) :: NameSet -> Bool
#if __GLASGOW_HASKELL__ <= 708
        postTcType = const (stage<TypeChecker)                 :: PostTcType -> Bool
#endif
        fixity     = const (stage<Renamer)                     :: GHC.Fixity -> Bool

-- | A variation of 'everything', using a 'GenericQ Bool' to skip
--   parts of the input 'Data'.
--everythingBut :: GenericQ Bool -> (r -> r -> r) -> r -> GenericQ r -> GenericQ r
--everythingBut q k z f x 
--  | q x       = z
--  | otherwise = foldl k (f x) (gmapQ (everythingBut q k z f) x)


-- Question: how to handle partial results in the otherwise step?
everythingButStaged :: Stage -> (r -> r -> r) -> r -> GenericQ (r,Bool) -> GenericQ r
everythingButStaged stage k z f x
  | (const False
#if __GLASGOW_HASKELL__ <= 708
       `extQ` postTcType
#endif
       `extQ` fixity `extQ` nameSet) x = z
  | stop == True = v
  | otherwise = foldl k v (gmapQ (everythingButStaged stage k z f) x)
  where (v, stop) = f x
        nameSet    = const (stage `elem` [Parser,TypeChecker]) :: NameSet -> Bool
#if __GLASGOW_HASKELL__ <= 708
        postTcType = const (stage<TypeChecker)                 :: PostTcType -> Bool
#endif
        fixity     = const (stage<Renamer)                     :: GHC.Fixity -> Bool

-- | Look up a subterm by means of a maybe-typed filter.
somethingStaged :: Stage -> (Maybe u) -> GenericQ (Maybe u) -> GenericQ (Maybe u)

-- "something" can be defined in terms of "everything"
-- when a suitable "choice" operator is used for reduction
--
somethingStaged stage z = everythingStaged stage orElse z


-- | Apply a monadic transformation at least somewhere.
--
-- The transformation is tried in a top-down manner and descends down if it
-- fails to apply at the root of the term.  If the transformation fails to apply
-- anywhere within the the term, the whole operation fails.
somewhereStaged :: MonadPlus m => Stage -> GenericM m -> GenericM m

somewhereStaged stage f x
  | (const False
#if __GLASGOW_HASKELL__ <= 708
       `extQ` postTcType
#endif
       `extQ` fixity `extQ` nameSet) x = mzero
  | otherwise = f x `mplus` gmapMp (somewhereStaged stage f) x
  where nameSet    = const (stage `elem` [Parser,TypeChecker]) :: NameSet -> Bool
#if __GLASGOW_HASKELL__ <= 708
        postTcType = const (stage<TypeChecker)                 :: PostTcType -> Bool
#endif
        fixity     = const (stage<Renamer)                     :: GHC.Fixity -> Bool

-- ---------------------------------------------------------------------

{-
-- | Apply a transformation everywhere in bottom-up manner
-- Note type GenericT = forall a. Data a => a -> a
everywhereStaged :: Stage
                    -> (forall a. Data a => a -> a)
                    -> (forall a. Data a => a -> a)

-- Use gmapT to recurse into immediate subterms;
-- recall: gmapT preserves the outermost constructor;
-- post-process recursively transformed result via f
--
everywhereStaged stage f -- = f . gmapT (everywhere f)
  | (const False `extQ` postTcType `extQ` fixity `extQ` nameSet) = mzero
  | otherwise = f . gmapT (everywhere stage f)
  where nameSet    = const (stage `elem` [Parser,TypeChecker]) :: NameSet -> Bool
        postTcType = const (stage<TypeChecker)                 :: PostTcType -> Bool
        fixity     = const (stage<Renamer)                     :: GHC.Fixity -> Bool
-}


-- | Monadic variation on everywhere
everywhereMStaged :: Monad m => Stage -> GenericM m -> GenericM m

-- Bottom-up order is also reflected in order of do-actions
everywhereMStaged stage f x
  | (const False
#if __GLASGOW_HASKELL__ <= 708
       `extQ` postTcType
#endif
       `extQ` fixity `extQ` nameSet) x = return x
  | otherwise = do x' <- gmapM (everywhereMStaged stage f) x
                   f x'
  where nameSet    = const (stage `elem` [Parser,TypeChecker]) :: NameSet -> Bool
#if __GLASGOW_HASKELL__ <= 708
        postTcType = const (stage<TypeChecker)                 :: PostTcType -> Bool
#endif
        fixity     = const (stage<Renamer)                     :: GHC.Fixity -> Bool


