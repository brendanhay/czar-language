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

import Control.Applicative ((<$>), (<*>))
import Control.Monad       (liftM)
import Czar.Language.AST
import Czar.Language.Lexer
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
-- import Text.Parsec.Language

expParser :: Parser Exp
expParser = buildExpressionParser table termParser <?> "expression"

table :: [a]
table =
    [
    ]

termParser :: Parser Exp
termParser =
        EVar <$> lowerIdent
    <|> letExpParser
    <|> literalParser
    <|> parenExpParser
    <|> listParser

letExpParser :: Parser Exp
letExpParser = do
    reserved "let"
    id' <- lowerIdent
    reservedOp "="
    ELet id' <$> expParser

listParser :: Parser Exp
listParser = list <$> brackets (commaSep expParser)

-- Parenthesised expression or a tuple
parenExpParser :: Parser Exp
parenExpParser = parenParser expParser tuple

parenParser :: Parser a -> ([a] -> a) -> Parser a
parenParser p tupler = do
    xs <- parens (commaSep1 p)
    return $ if length xs == 1
              then head xs
              else tupler xs

literalParser :: Parser Exp
literalParser =
        boolParser
    <|> numParser
    <|> (litChar   <$> charLiteral)
    <|> (litString <$> stringLiteral)
    <|> (litChar   <$> charLiteral)
    <|> (litString <$> stringLiteral)

boolParser :: Parser Exp
boolParser = litBool <$> (true <|> false)

numParser :: Parser Exp
numParser = either litInt litFloat <$> naturalOrFloat
