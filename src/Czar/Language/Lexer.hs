-- |
-- Module      : Czar.Language.Lexer
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Czar.Language.Lexer (
      lexer
    ) where

import Data.Functor.Identity (Identity)
import Text.Parsec
import Text.Parsec.Language  (emptyDef)

import qualified Text.Parsec.Token as P

type Lex a b = ParsecT String a Identity b

lexer :: P.GenTokenParser String a Identity
lexer = P.makeTokenParser rules

rules :: P.GenLanguageDef String u Identity
rules = emptyDef
    { P.commentLine     = "--"
    , P.nestedComments  = True
    , P.caseSensitive   = True
    , P.identStart      = letter
    , P.identLetter     = alphaNum
    , P.reservedNames   = reservedNames
    , P.reservedOpNames = reservedOps
    }

reservedNames :: [String]
reservedNames =
    [ "module"
    , "manifest"
    , "provider"
    , "resource"
    , "where"
    , "perform"
    , "include"
    , "let"
    , "if"
    , "unless"
    , "then"
    , "else"
    , "case"
    , "of"
    , "_"
    ]

reservedOps :: [String]
reservedOps =
    [ "="
    , "->"
    , ">"
    , "<"
    , "+"
    , "-"
    , "*"
    , "/"
    ]

identifier :: Lex a String
identifier = P.identifier lexer

reserved :: String -> Lex a ()
reserved = P.reserved lexer

operator :: Lex a String
operator = P.operator lexer

reservedOp :: String -> Lex a ()
reservedOp = P.reservedOp lexer

charLiteral :: Lex a Char
charLiteral = P.charLiteral lexer

stringLiteral :: Lex a String
stringLiteral = P.stringLiteral lexer

natural :: Lex a Integer
natural = P.natural lexer

integer :: Lex a Integer
integer = P.integer lexer

float :: Lex a Double
float = P.float lexer

naturalOrFloat :: Lex a (Either Integer Double)
naturalOrFloat = P.naturalOrFloat lexer

decimal :: Lex a Integer
decimal = P.decimal lexer

hexadecimal :: Lex a Integer
hexadecimal = P.hexadecimal lexer

octal :: Lex a Integer
octal = P.octal lexer

symbol :: String -> Lex a String
symbol = P.symbol lexer

lexeme :: Lex a b -> Lex a b
lexeme = P.lexeme lexer

whiteSpace :: Lex a ()
whiteSpace = P.whiteSpace lexer

parens :: Lex a b -> Lex a b
parens = P.parens lexer

braces :: Lex a b -> Lex a b
braces = P.braces lexer

angles :: Lex a b -> Lex a b
angles = P.angles lexer

brackets :: Lex a b -> Lex a b
brackets = P.brackets lexer
