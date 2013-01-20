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
    | ELet Ident Exp
    | ELit Literal
    | ETuple [Exp]
    | EApp Exp Exp
    | ENeg Exp
    | EBin BinOp Exp Exp
    | ERel RelOp Exp Exp
    | ENum NumOp Exp Exp
    | EIf Exp Exp Exp
    | ECase Exp [(Pattern, Exp)]
  deriving (Show)

data Pattern
    = PVar Ident
    | PNeg Pattern
    | PLit Literal
    | PWildCard
  deriving (Show)

data Literal
    = LChar Char
    | LString String
    | LBool Bool
    | LInt Integer
    | LFloat Double
    | LCons
    | LNil
  deriving (Show)

data BinOp = And | Or
  deriving (Show)

data RelOp = Greater | Less
  deriving (Show)

data NumOp
    = Add
    | Subtract
    | Multiply
    | Divide
  deriving (Show)

infixl 9 @@
(@@) :: Exp -> Exp -> Exp
e1 @@ e2 = EApp e1 e2

litInt :: Integer -> Exp
litInt = ELit . LInt

litFloat :: Double -> Exp
litFloat = ELit . LFloat

litChar :: Char -> Exp
litChar = ELit . LChar

litString :: String -> Exp
litString = ELit . LString

litBool :: Bool -> Exp
litBool = ELit . LBool

litCons :: Exp
litCons = ELit LCons

litNil :: Exp
litNil = ELit LNil

var :: Ident -> Exp
var = EVar

list :: [Exp] -> Exp
list = foldr cons litNil

cons :: Exp -> Exp -> Exp
cons hd tl = litCons @@ hd @@ tl

tuple :: [Exp] -> Exp
tuple = ETuple
