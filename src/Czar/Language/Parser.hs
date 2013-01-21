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

qualNameParser :: Parser QName
qualNameParser = QName <$> sepBy1 upperIdent dot

refNameParser :: Parser RName
refNameParser = RName <$> qualNameParser <*> lowerIdent

manifestParser :: Parser Manifest
manifestParser = do
    reserved "manifest"
    name <- qualNameParser
    args <- option [] $ parens (commaSep argParser)
    binding
    return $ Manifest name args [] [] []

argParser :: Parser Arg
argParser = do
    name <- lowerIdent
    choice $ map ($ name) [typeArg, refArg, litArg]

-- FIXME: Tidy these three up
litArg :: String -> Parser Arg
litArg s = try $ reservedOp "=" >> ALit s <$> literalParser

refArg :: String -> Parser Arg
refArg s = try $ reservedOp "=" >> ARef s <$> refNameParser

typeArg :: String -> Parser Arg
typeArg s = reservedOp "::" >> ASig s <$> typeParser

typeParser :: Parser Type
typeParser = ctorType <|> tupleType <|> listType

listType :: Parser Type
listType = TList <$> brackets typeParser

tupleType :: Parser Type
tupleType = parenParser typeParser TTuple

ctorType :: Parser Type
ctorType = do
    name <- upperIdent
    case name of
        "Char"   -> return TChar
        "String" -> return TString
        "Bool"   -> return TBool
        "Int"    -> return TInt
        "Float"  -> return TFloat
        _        -> fail $ "unexpected type constructor: " ++ name

parenParser :: ParseT a b -> ([b] -> b) -> ParsecT String a Identity b
parenParser p ctor = do
    xs <- parens (commaSep1 p)
    return $ if length xs == 1
              then head xs
              else ctor xs

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
    name <- lowerIdent
    reservedOp "="
    ELet name <$> expParser

listExp :: Parser Exp
listExp = list <$> brackets (commaSep expParser)

parenExp :: Parser Exp
parenExp = parenParser expParser ETuple

literalExp :: Parser Exp
literalExp = ELit <$> literalParser

literalParser :: Parser Literal
literalParser = boolLit <|> numLit <|> charLit <|> stringLit

boolLit :: Parser Literal
boolLit = LBool <$> (true <|> false)

numLit :: Parser Literal
numLit = either LInt LFloat <$> naturalOrFloat

charLit :: Parser Literal
charLit = LChar <$> charLiteral

stringLit :: Parser Literal
stringLit = LString <$> stringLiteral
