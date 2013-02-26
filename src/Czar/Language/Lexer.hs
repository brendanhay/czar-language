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

import Data.Char                     (isUpper)
import Control.Monad.Identity
import Text.Parsec
import Text.Parsec.IndentParsec.Prim
import Text.Parsec.Language

import qualified Text.Parsec.Token              as P
import qualified Text.Parsec.IndentParsec.Token as I

lexer :: I.IndentTokenParser String () Identity
lexer = P.makeTokenParser rules

rules :: P.GenLanguageDef String u (IndentT HaskellLike Identity)
rules = P.LanguageDef
    { P.commentStart    = ""
    , P.commentEnd      = ""
    , P.commentLine     = "--"
    , P.nestedComments  = False
    , P.identStart      = letter <|> char '_'
    , P.identLetter     = alphaNum <|> oneOf "_'"
    , P.opStart         = P.opLetter rules
    , P.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , P.reservedOpNames = reservedOps
    , P.reservedNames   = reservedNames
    , P.caseSensitive   = True
    }

reservedNames :: [String]
reservedNames =
    [ "module"
    , "manifest"
    , "provider"
    , "resource"
    , "include"
    , "provide"
    , "where"
    , "var"
    , "if"
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
    , "::"
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

-- FIXME: Must be a better way of coercing parsec to
-- split lower/upper idents
upperIdent = upperOrLowerIdent >>= either
    return (fail . ("expecting uppercase ident: " ++))

lowerIdent = upperOrLowerIdent >>= either
    (fail . ("expecting lowercase ident: " ++)) return

upperOrLowerIdent = do
    xs <- I.identifier lexer
    return $ if isUpper (head xs)
              then Left xs
              else Right xs

reserved       = I.reserved lexer
operator       = I.operator lexer
reservedOp     = I.reservedOp lexer
charLiteral    = I.charLiteral lexer
stringLiteral  = I.stringLiteral lexer
natural        = I.natural lexer
integer        = I.integer lexer
float          = I.float lexer
naturalOrFloat = I.naturalOrFloat lexer
decimal        = I.decimal lexer
hexadecimal    = I.hexadecimal lexer
octal          = I.octal lexer
symbol         = I.symbol lexer
lexeme         = I.lexeme lexer
whiteSpace     = I.whiteSpace lexer
parens         = I.parens lexer
braces         = I.braces lexer
angles         = I.angles lexer
brackets       = I.brackets lexer
commaSep       = I.commaSep lexer
commaSep1      = I.commaSep1 lexer
dot            = I.dot lexer

--true, false :: ParseT a Bool
true  = res "true" True
false = res "false" False

--bNot, bAnd, bOr :: a -> ParseT b a
bNot = op "!"
bAnd = op "&&"
bOr  = op "||"

--rGreater, rLess :: a -> ParseT b a
rGreater = op ">"
rLess    = op "<"

-- aAdd, aSubtract, aMultiply, aDivide :: a -> ParseT b a
aAdd      = op "+"
aSubtract = op "-"
aMultiply = op "*"
aDivide   = op "/"

-- res, op :: String -> a -> ParseT b a
res s x = reserved s >> return x
op s x  = reservedOp s >> return x
