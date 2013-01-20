-- |
-- Module      : Czar.Language.Parser
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Czar.Language.Parser where

import Control.Applicative   ((<$>), (<*>))
import Czar.Language.AST
import Czar.Language.Lexer
import Data.Functor.Identity (Identity)
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr

type Operators = [[Operator String () Identity Exp]]

expParser :: Parser Exp
expParser = buildExpressionParser operators termParser <?> "expression"

operators :: Operators
operators = arithmeticOps ++ booleanOps ++ relationalOps

arithmeticOps :: Operators
arithmeticOps =
    [ [Prefix (aSubtract ENeg)]
    , [Infix  (aAdd      (ENum Add))      AssocLeft]
    , [Infix  (aSubtract (ENum Subtract)) AssocLeft]
    , [Infix  (aMultiply (ENum Multiply)) AssocLeft]
    , [Infix  (aDivide   (ENum Divide))   AssocLeft]
    ]

booleanOps :: Operators
booleanOps =
    [ [Prefix (bNot ENeg)]
    , [Infix  (bAnd (EBin And)) AssocLeft]
    , [Infix  (bOr  (EBin Or))  AssocLeft]
    ]

relationalOps :: Operators
relationalOps =
    [ [Infix (rGreater (ERel Greater)) AssocLeft]
    , [Infix (rLess    (ERel Less))    AssocLeft]
    ]

termParser :: Parser Exp
termParser = EVar <$> lowerIdent
    <|> letExpParser
    <|> literalParser
    <|> parenParser
    <|> listParser

letExpParser :: Parser Exp
letExpParser = do
    reserved "let"
    id' <- lowerIdent
    reservedOp "="
    ELet id' <$> expParser

listParser :: Parser Exp
listParser = list <$> brackets (commaSep expParser)

parenParser :: Parser Exp
parenParser = do
    xs <- parens (commaSep1 expParser)
    return $ if length xs == 1
              then head xs
              else tuple xs

literalParser :: Parser Exp
literalParser = boolParser
    <|> numParser
    <|> charParser
    <|> stringParser

boolParser :: Parser Exp
boolParser = litBool <$> (true <|> false)

numParser :: Parser Exp
numParser = either litInt litFloat <$> naturalOrFloat

charParser :: Parser Exp
charParser = litChar <$> charLiteral

stringParser :: Parser Exp
stringParser = litString <$> stringLiteral
