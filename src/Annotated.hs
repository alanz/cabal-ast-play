{-

Experimenting with decorating the potential AST with annotations.

Initially, this will be a separate version of the AST completely, contained only in this file.

It is based diectly on a proposal by Iavor Diatchki as a tweak on the
IndexedProduct annotations proposed by Shayan Najd in his Haskell Summer of Code
project.


-}

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

data GenericPackageDescription = GenericPackageDescription {
  flags       :: [Flag],
  pkgCondTree :: ConditionTree Predicate BuildInfo
}

data Flag = Flag {
    flagName        :: FlagName,
    flagDefault     :: Bool,
    flagManual      :: Bool
  }
  deriving (Show, Eq)

data BuildInfo = BuildInfo {
    buildDepends      :: [Dependency],
    pkgconfigDepends  :: [Dependency],
    extralibs         :: [String]
  }

-- | A condition tree is a tree of values guarded by condition expressions.
-- Each level of the tree has a list of nodes.
--
newtype ConditionTree condition body
      = ConditionTree [ConditionNode condition body]
  deriving (Eq, Show)

-- An individual condition tree node is either a body value or a conditional
-- with a further tree below. Conditionals can have an else part.
--
data ConditionNode condition body
   = Body             body
   | IfThen condition (ConditionTree condition body)
   | IfElse condition (ConditionTree condition body)
                      (ConditionTree condition body)
  deriving (Eq, Show)

type Predicate = PredicateExpr BasePredicate

data PredicateExpr base
   = PredExprBase base
   | PredExprLit  Bool
   | PredExprNot  (PredicateExpr base)
   | PredExprAnd  (PredicateExpr base) (PredicateExpr base)
   | PredExprOr   (PredicateExpr base) (PredicateExpr base)
  deriving (Eq, Show)


data BasePredicate
   = OSPredicate       OS
   | ArchPredicate     Arch
   | CompilerPredicate CompilerFlavor VersionRange
   | FlagPredicate     FlagName
  deriving (Eq, Show)
