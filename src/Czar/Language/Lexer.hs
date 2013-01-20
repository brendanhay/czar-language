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

module Czar.Language.Lexer where

import Data.Functor.Identity (Identity)
import Text.Parsec
import Text.Parsec.Language  (emptyDef)

import qualified Text.Parsec.Token as P

type ParseT a b = ParsecT String a Identity b

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
    , "true"
    , "false"
    ]

reservedOps :: [String]
reservedOps =
    [ "="
    , "->"
    , ">"
    , ">="
    , "<"
    , "=<"
    , "+"
    , "-"
    , "*"
    , "/"
    , "=~"
    , "~~"
    , "=="
    , "!="
    , "||"
    , "&&"
    ]

identifier :: ParseT a String
identifier = P.identifier lexer

reserved :: String -> ParseT a ()
reserved = P.reserved lexer

operator :: ParseT a String
operator = P.operator lexer

reservedOp :: String -> ParseT a ()
reservedOp = P.reservedOp lexer

charLiteral :: ParseT a Char
charLiteral = P.charLiteral lexer

stringLiteral :: ParseT a String
stringLiteral = P.stringLiteral lexer

natural :: ParseT a Integer
natural = P.natural lexer

integer :: ParseT a Integer
integer = P.integer lexer

float :: ParseT a Double
float = P.float lexer

naturalOrFloat :: ParseT a (Either Integer Double)
naturalOrFloat = P.naturalOrFloat lexer

decimal :: ParseT a Integer
decimal = P.decimal lexer

hexadecimal :: ParseT a Integer
hexadecimal = P.hexadecimal lexer

octal :: ParseT a Integer
octal = P.octal lexer

symbol :: String -> ParseT a String
symbol = P.symbol lexer

lexeme :: ParseT a b -> ParseT a b
lexeme = P.lexeme lexer

whiteSpace :: ParseT a ()
whiteSpace = P.whiteSpace lexer

parens :: ParseT a b -> ParseT a b
parens = P.parens lexer

braces :: ParseT a b -> ParseT a b
braces = P.braces lexer

angles :: ParseT a b -> ParseT a b
angles = P.angles lexer

brackets :: ParseT a b -> ParseT a b
brackets = P.brackets lexer

commaSep :: ParseT a b -> ParseT a [b]
commaSep = P.commaSep lexer

commaSep1 :: ParseT a b -> ParseT a [b]
commaSep1 = P.commaSep1 lexer

true, false :: ParseT a Bool
true  = res "true" True
false = res "false" False

tNot, tAnd, tOr, tGreater, tLess :: a -> ParseT b a
tNot     = op "!"
tAnd     = op "&&"
tOr      = op "||"
tGreater = op ">"
tLess    = op "<"

res, op :: String -> a -> ParseT b a
res = f reserved
op  = f reservedOp

f :: Monad m => (a -> m b) -> a -> c -> m c
f g h x = g h >> return x
