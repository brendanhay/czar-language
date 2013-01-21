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
import Control.Monad         (mzero)
import Czar.Language.AST
import Czar.Language.Lexer
import Data.Functor.Identity (Identity)
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr

type Operators = [[Operator String () Identity Exp]]

expParser :: Parser Exp
expParser = buildExpressionParser operators appExp <?> "expression"

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

appExp :: Parser Exp
appExp = do
   es <- many1 termExp
   case length es of
      0 -> mzero
      1 -> return $ head es
      _ -> return $ foldl1 EApp es

termExp :: Parser Exp
termExp = EVar <$> lowerIdent
    <|> letExp
    <|> literalExp
    <|> parenExp
    <|> listExp

letExp :: Parser Exp
letExp = do
    reserved "let"
    id' <- lowerIdent
    reservedOp "="
    ELet id' <$> expParser

listExp :: Parser Exp
listExp = list <$> brackets (commaSep expParser)

parenExp :: Parser Exp
parenExp = do
    xs <- parens (commaSep1 expParser)
    return $ if length xs == 1
              then head xs
              else tuple xs

literalExp :: Parser Exp
literalExp = boolExp
    <|> numExp
    <|> charExp
    <|> stringExp

boolExp :: Parser Exp
boolExp = litBool <$> (true <|> false)

numExp :: Parser Exp
numExp = either litInt litFloat <$> naturalOrFloat

charExp :: Parser Exp
charExp = litChar <$> charLiteral

stringExp :: Parser Exp
stringExp = litString <$> stringLiteral
