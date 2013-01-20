-- |
-- Module      : Czar.Language.AST
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Czar.Language.AST where

-- import Data.String

type Ident = String

newtype ModName = ModName Ident
  deriving (Show)

data RefName = RefName ModName Ident
  deriving (Show)

data Module = Module ModName [Manifest] [Exp]
  deriving (Show)

data Manifest = Manifest ModName [Arg] [Perform] [Include] [Decl]
  deriving (Show)

data Type
    = TInt
    | TFloat
    | TChar
    | TString
    | TBool
    | TList Type
    | TTuple [Type]
  deriving (Eq, Show)

data Arg
    = AExp Ident Exp
    | ASig Ident Type
    | ARef Ident RefName
  deriving (Show)

data Perform = Perform ModName [Decl]
  deriving (Show)

data Include = Include ModName [Decl]
  deriving (Show)

data Decl
    = DRes Ident Ident [Stmt]
    | DStmt Stmt
  deriving (Show)

data Stmt
    = SLet Ident Exp
    | SIf Exp Stmt Stmt
    | SCase Exp [(Pattern, Stmt)]
  deriving (Show)

data Exp
    = EVar Ident
    | ERef RefName
    | ELet Ident Exp
    | ELit Literal
    | EBin BinExp
    | ENum NumExp
    | EList [Exp]
    | ETuple [Exp]
    | EIf Exp Exp Exp
    | ECase Exp [(Pattern, Exp)]
  deriving (Show)

refExp :: Ident -> Ident -> Exp
refExp mod' = ERef . RefName (ModName mod')

boolExp :: Bool -> Exp
boolExp = EBin . BBool

data Pattern
    = PVar Ident
    | PNeg Pattern
    | PLit Literal
    | PWildCard
  deriving (Show)

data Literal
    = LChar Char
    | LString String
    | LList [Literal]
    | LTuple [Literal]
  deriving (Show)

data BinExp
    = BBool Bool
    | BNeg BinExp
    | BExp BinOp BinExp BinExp
    | RExp RelOp NumExp NumExp
  deriving (Show)

data BinOp = And | Or
  deriving (Show)

data RelOp = Greater | Less
  deriving (Show)

data NumExp
    = NInt Integer
    | NFloat Double
    | NNeg NumExp
    | NExp NumOp NumExp NumExp
  deriving (Show)

data NumOp
    = Add
    | Subtract
    | Multiply
    | Divide
  deriving (Show)

