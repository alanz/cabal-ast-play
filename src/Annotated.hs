{-

Experimenting with decorating the potential AST with annotations.

Initially, this will be a separate version of the AST completely, contained only in this file.

It is based diectly on a proposal by Iavor Diatchki as a tweak on the
IndexedProduct annotations proposed by Shayan Najd in his Haskell Summer of Code
project.


-}
{-# LANGUAGE DataKinds, TypeFamilies, PolyKinds #-}

-- The following are needed for constraints around Eq and Show instances
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}


import Distribution.PackageDescription
         ( FlagName, FlagAssignment )

import Distribution.Package
         ( Dependency(..), PackageName )

import Distribution.System
         ( OS(..), Arch(..) )

import Distribution.Compiler
         ( CompilerFlavor, CompilerId(..) )

import Distribution.Version
         ( VersionRange(..), withinRange )

-- ---------------------------------------------------------------------

import Data.Void(Void)        -- Used in examples
import GHC.Prim(Constraint)   -- Used for "generic deriving"

-- Basics

{-
Tags
----

We declare a collection of tags, which are used to specify the various ways
in which the syntax may be extended.  The comment `{-kind-}` has no special
meaning---it just indicates that we intend to use only the promoted versions
of these types.
-}

data {-kind-} GenericPackageDescriptionTag = Con_GPD
data {-kind-} FlagTag           = Con_Flag
data {-kind-} BuildInfoTag      = Con_BuildInfo
data {-kind-} ConditionTreeTag  = Con_ConditionTree
data {-kind-} ConditionNodeTag  = Con_Body | Con_IfThen | Con_IfElse
data {-kind-} PredicateExprTag  = Con_PredExprBase | Con_PredExprLit | Con_PredExprNot
                                | Con_PredExprAnd | Con_PredExprOr

data {-kind-} BasePredicateTag  = Con_OSPredicate | Con_ArchPredicate | Con_CompilerPredicate
                                | Con_FlagPredicate

{-
For each syntactic category we add an entry to `TyTag`, and we
also create a separate collection of tags, one for each constructor
in that category.  The tags starting with `Ty_` are used to add additionalA
constructors to a syntactic category.
The tags starting with `Con_` are used to add additional fields to
a constructor.

The actual types used to extend a particular type are computed
by a type family parametrized by the name of an instantiation and a tag:
-}

type family Ext syn (tag :: k)

{-
The polymorphic kind allows us to use the family with any of
`TyTag`, `ExpTag`, etc.

We may also compute other types for each particular instantiation.
For example, the type family `Ident` is used to determine the type of
identifiers in a particular instantiation:
-}

type family Ident syn

-- ---------------------------------------------------------------------

data GenericPackageDescription syn = GenericPackageDescription {
  ann         :: Ext syn 'Con_GPD,
  flags       :: [Flag syn],
  pkgCondTree :: ConditionTree syn (Predicate syn) (BuildInfo syn)
}
deriving instance (All_In Show syn) => Show (GenericPackageDescription syn)

data Flag syn = Flag {
    flagAnn         :: Ext syn 'Con_Flag,
    flagName        :: FlagName,
    flagDefault     :: Bool,
    flagManual      :: Bool
  }
  -- deriving (Show, Eq)
deriving instance (All_FlagTag_In Eq syn)   => Eq (Flag syn)
deriving instance (All_FlagTag_In Show syn) => Show (Flag syn)

data BuildInfo syn = BuildInfo {
    buildAnn          :: Ext syn 'Con_BuildInfo,
    buildDepends      :: [Dependency],
    pkgconfigDepends  :: [Dependency],
    extralibs         :: [String]
  }
deriving instance (All_In Show syn) => Show (BuildInfo syn)

-- | A condition tree is a tree of values guarded by condition expressions.
-- Each level of the tree has a list of nodes.
--
data ConditionTree syn condition body
      = ConditionTree (Ext syn 'Con_ConditionTree) [ConditionNode syn condition body]
deriving instance (Eq condition, Eq body,
                   All_In Eq syn) => Eq (ConditionTree syn condition body)
deriving instance (Show condition, Show body,
                   All_In Show syn) => Show (ConditionTree syn condition body)

-- An individual condition tree node is either a body value or a conditional
-- with a further tree below. Conditionals can have an else part.
--
data ConditionNode syn condition body
   = Body             (Ext syn 'Con_Body) body
   | IfThen condition (Ext syn 'Con_IfThen)
                      (ConditionTree syn condition body)
   | IfElse condition (Ext syn 'Con_IfElse)
                      (ConditionTree syn condition body)
                      (ConditionTree syn condition body)
deriving instance (Eq condition, Eq body,
                   All_In Eq syn) => Eq (ConditionNode syn condition body)
deriving instance (Show condition, Show body,
                   All_In Show syn) => Show (ConditionNode syn condition body)

type Predicate syn = PredicateExpr syn (BasePredicate syn)

data PredicateExpr syn base
   = PredExprBase (Ext syn 'Con_PredExprBase) base
   | PredExprLit  (Ext syn 'Con_PredExprLit) Bool
   | PredExprNot  (Ext syn 'Con_PredExprNot) (PredicateExpr syn base)
   | PredExprAnd  (Ext syn 'Con_PredExprAnd) (PredicateExpr syn base) (PredicateExpr syn base)
   | PredExprOr   (Ext syn 'Con_PredExprOr)  (PredicateExpr syn base) (PredicateExpr syn base)
deriving instance (Eq base,All_PredicateExprTag_In Eq syn)   => Eq (PredicateExpr syn base)
deriving instance (Show base,All_PredicateExprTag_In Show syn) => Show (PredicateExpr syn base)


data BasePredicate syn
   = OSPredicate       (Ext syn 'Con_OSPredicate) OS
   | ArchPredicate     (Ext syn 'Con_ArchPredicate) Arch
   | CompilerPredicate (Ext syn 'Con_CompilerPredicate) CompilerFlavor VersionRange
   | FlagPredicate     (Ext syn 'Con_FlagPredicate) FlagName
deriving instance (All_BasePredicateTag_In Eq syn)   => Eq   (BasePredicate syn)
deriving instance (All_BasePredicateTag_In Show syn) => Show (BasePredicate syn)

-- ---------------------------------------------------------------------

-- Messy boilerplate, which also forces use of UndecidableInstances. Safe in
-- this case though.

type All_BasePredicateTag_In (c :: * -> Constraint) syn =
  (
    c (Ext syn 'Con_OSPredicate)
  , c (Ext syn 'Con_ArchPredicate)
  , c (Ext syn 'Con_CompilerPredicate)
  , c (Ext syn 'Con_FlagPredicate)
  )


type All_PredicateExprTag_In (c :: * -> Constraint) syn =
  (
    c (Ext syn 'Con_PredExprBase)
  , c (Ext syn 'Con_PredExprLit)
  , c (Ext syn 'Con_PredExprNot)
  , c (Ext syn 'Con_PredExprAnd)
  , c (Ext syn 'Con_PredExprOr)
  )

type All_ConditionNodeTag_In (c :: * -> Constraint) syn =
  (
    c (Ext syn 'Con_Body)
  , c (Ext syn 'Con_IfThen)
  , c (Ext syn 'Con_IfElse)
  )

-- keep the regularity,even though only one constructor
type All_ConditionTreeTag_In (c :: * -> Constraint) syn =
  (
    c (Ext syn 'Con_ConditionTree)
  )

-- keep the regularity,even though only one constructor
type All_FlagTag_In (c :: * -> Constraint) syn =
  (
    c (Ext syn 'Con_Flag)
  )

-- keep the regularity,even though only one constructor
type All_BuildInfoTag_In (c :: * -> Constraint) syn =
  (
    c (Ext syn 'Con_BuildInfo)
  )

-- keep the regularity,even though only one constructor
type All_GenericPackageDescriptionTag_In (c :: * -> Constraint) syn =
  (
    c (Ext syn 'Con_GPD)
  )

-- The roll-up of all constraints
type All_In (c :: * -> Constraint) syn =
  ( All_ConditionTreeTag_In c syn
  , All_ConditionNodeTag_In c syn
  , All_PredicateExprTag_In c syn
  , All_BasePredicateTag_In c syn
  , All_FlagTag_In c syn
  , All_BuildInfoTag_In c syn
  , All_GenericPackageDescriptionTag_In c syn
  )

-- ---------------------------------------------------------------------

-- Structure in place, time for some concrete types

-- Minimal annotations, just using ()
data AnnBare -- Index type for the specific family of annotations

type instance Ext   AnnBare (t :: GenericPackageDescriptionTag) = ()
type instance Ext   AnnBare (t :: ConditionTreeTag)             = ()

eg_gpd_bare :: GenericPackageDescription AnnBare
eg_gpd_bare = GenericPackageDescription
  { ann = ()
  , flags = []
  , pkgCondTree = ConditionTree () []
  }

-- ---------------------------------------------------------------------

-- Annotations to capture locations, used for initial parse of the cabal file
data AnnLoc -- Index type for the specific family of annotations

-- WIP. Need insight into how the package is parsed
type instance Ext   AnnLoc (t :: GenericPackageDescriptionTag) = ()
type instance Ext   AnnLoc (t :: ConditionTreeTag)             = ()
type SrcSpan = ((Int,Int),(Int,Int))

eg_gpd_loc :: GenericPackageDescription AnnLoc
eg_gpd_loc = GenericPackageDescription
  { ann = ()
  , flags = []
  , pkgCondTree = ConditionTree () []
  }
