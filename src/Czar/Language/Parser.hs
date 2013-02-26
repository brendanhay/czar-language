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

import Control.Applicative                        ((<$>), (<*>))
import Control.Monad
import Control.Monad.Identity
import Czar.Language.AST
import Czar.Language.Lexer
import Text.Parsec                         hiding (between)
import Text.Parsec.Expr
import Text.Parsec.IndentParsec.Combinator
import Text.Parsec.IndentParsec.Prim

type Operators = [Operator String () (IndentT HaskellLike Identity) Exp]

type Parser a = IndentParsecT String () Identity a

qnameParser :: Parser QName
qnameParser = QName <$> sepBy1 upperIdent dot

rnameParser :: Parser RName
rnameParser = RName <$> qnameParser <*> lowerIdent

manifestParser :: Parser Manifest
manifestParser = do
    whiteSpace
    reserved "manifest"
    n  <- qnameParser
    as <- bindingParser
    is <- many importParser
    es <- many expParser
    eof
    return $ Manifest n as is es

importParser :: Parser Decl
importParser = Decl "import"
    <$> (reserved "import" >> qnameParser)
    <*> bindingParser

bindingParser :: Parser [Bind]
bindingParser = option [] $ reserved "where" >> blockOf (many1 argParser)

argParser :: Parser Bind
argParser = do
    n <- lowerIdent
    choice $ map ($ n) [typeBind, refBind, expBind]

typeBind :: String -> Parser Bind
typeBind s = reservedOp "::" >> ASig s <$> typeParser

refBind :: String -> Parser Bind
refBind s = try $ reservedOp "=" >> ARef s <$> rnameParser

expBind :: String -> Parser Bind
expBind s = try $ reservedOp "=" >> AExp s <$> blockOf expParser

typeParser :: Parser Type
typeParser = ctorType <|> tupleType <|> listType

listType :: Parser Type
listType = TList <$> brackets typeParser

tupleType :: Parser Type
tupleType = parenParser typeParser TTuple

ctorType :: Parser Type
ctorType = do
    n <- upperIdent
    case n of
        "Char"   -> return TChar
        "String" -> return TString
        "Bool"   -> return TBool
        "Int"    -> return TInt
        "Float"  -> return TFloat
        _        -> fail $ "unexpected type constructor: " ++ n

parenParser :: GenIndentParsecT HaskellLike String () Identity a
            -> ([a] -> a)
            -> ParsecT String () (IndentT HaskellLike Identity) a
parenParser p ctor = do
    xs <- parens (commaSep1 p)
    return $ if length xs == 1
              then head xs
              else ctor xs

expParser :: Parser Exp
expParser = opParser (letExp <|> condExp <|> caseExp <|> appExp)

letExp :: Parser Exp
letExp = do
    reserved "let"
    n <- lowerIdent
    reservedOp "="
    e <- termExp
    return $ ELet n e

condExp :: Parser Exp
condExp = ECond
    <$> (reserved "if" >> opParser termExp)
    <*> branch "then"
    <*> branch "else"
  where
    branch w = reserved w >> expParser

caseExp :: Parser Exp
caseExp = ECase
    <$> (reserved "case" >> opParser termExp)
    <*> (reserved "of"   >> many1 matchParser)

matchParser :: Parser (Pattern, Exp)
matchParser = (,) <$> patternParser <*> (reserved "->" >> termExp)

patternParser :: Parser Pattern
patternParser = varPat <|> negPat <|> litPat <|> wildPat

varPat :: Parser Pattern
varPat = PVar <$> lowerIdent

negPat :: Parser Pattern
negPat = PNeg <$> (reserved "!" >> patternParser)

litPat :: Parser Pattern
litPat = PLit <$> literalParser

wildPat :: Parser Pattern
wildPat = reserved "_" >> return PWildCard

appExp :: Parser Exp
appExp = do
    es <- many1 termExp
    bs <- bindingParser
    case length es of
        0 -> mzero
        1 -> return $ head es
        _ -> return $ foldl1 (f bs) es
  where
    f xs x y = EApp x y xs

termExp :: Parser Exp
termExp = varExp <|> litExp <|> parenExp <|> listExp

varExp :: Parser Exp
varExp = EVar <$> lowerIdent

listExp :: Parser Exp
listExp = list <$> brackets (commaSep expParser)

parenExp :: Parser Exp
parenExp = parenParser expParser ETuple

litExp :: Parser Exp
litExp = ELit <$> literalParser

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

opParser :: Parser Exp -> Parser Exp
opParser p = buildExpressionParser ops p <?> "expression"
  where
    ops = arithmeticOps ++ booleanOps ++ relationalOps

arithmeticOps :: [Operators]
arithmeticOps =
    [ [Prefix (aSubtract ENeg)]
    , [Infix  (aAdd      (ENum Add))      AssocLeft]
    , [Infix  (aSubtract (ENum Subtract)) AssocLeft]
    , [Infix  (aMultiply (ENum Multiply)) AssocLeft]
    , [Infix  (aDivide   (ENum Divide))   AssocLeft]
    ]

booleanOps :: [Operators]
booleanOps =
    [ [Prefix (bNot ENeg)]
    , [Infix  (bAnd (EBin And)) AssocLeft]
    , [Infix  (bOr  (EBin Or))  AssocLeft]
    ]

relationalOps :: [Operators]
relationalOps =
    [ [Infix (rGreater (ERel Greater)) AssocLeft]
    , [Infix (rLess    (ERel Less))    AssocLeft]
    ]
