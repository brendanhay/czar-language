-- |
-- Module      : Czar.Language.Formatter
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Czar.Language.Formatter (
      render
    ) where

import Prelude
import Czar.Language.AST            hiding (list)
import Data.List                           (intersperse, intercalate)
import Data.Text.Lazy                      (Text)
import Text.PrettyPrint.Leijen.Text

import qualified Data.Text.Lazy as T

render :: Format a => a -> Text
render = displayT . renderPretty 0.4 100 . fmt

class Format a where
    fmt     :: a -> Doc
    fmtPrec :: Int -> a -> Doc

    fmt       = fmtPrec 0
    fmtPrec _ = fmt

instance Format QName where
    fmt (QName is) = ident $ intercalate "." is

instance Format RName where
    fmt (RName q i) = fmt q <$> ident i

instance Format Manifest where
    fmt (Manifest q bs ds es) = m <$> vfmt ds <$> vfmt es
      where
        m = block $ text "manifiest" <+> fmt q <$> fmt bs

instance Format Decl where
    fmt (Decl i q bs) = block $ ident i <+> fmt q <$> fmt bs

instance Format [Bind] where
    fmt [] = empty
    fmt bs = nest 2 . (text "where" <$>) . vfmt $ zip (repeat m) bs
      where
        m = foldl max 0 $ map (length . bindIdent) bs

instance Format (Int, Bind) where
    fmt (n, AExp i exp)   = binding n i equals exp
    fmt (n, ARef i rname) = binding n i equals rname
    fmt (n, ASig i typ)   = binding n i (colon <> colon) typ

instance Format Type where
    fmt TInt        = text "Int"
    fmt TFloat      = text "Float"
    fmt TChar       = text "Char"
    fmt TString     = text "String"
    fmt TBool       = text "Bool"
    fmt (TList typ) = list [fmt typ]
    fmt (TTuple ts) = tupled $ map fmt ts

instance Format Literal where
    fmt (LChar c)   = squotes $ char c
    fmt (LString s) = dquotes . string $ T.pack s
    fmt (LBool b)   = text . T.toLower . T.pack $ show b
    fmt (LInt i)    = integer i
    fmt (LFloat d)  = double d
    fmt LCons       = lbracket <> rbracket
    fmt LNil        = lbracket <> rbracket

instance Format Exp where
    fmt (EVar i)       = ident i
    fmt (ELet i exp)   = text "let" <+> binding 0 i equals exp
    fmt (ELit lit)     = fmt lit
    fmt (ETuple es)    = tupled $ map fmt es
    fmt (EApp x y bs)  = fmt x <+> fmt y <$> fmt bs
    fmt (ENeg exp)     = char '!' <> fmt exp
    fmt (EBin bop x y) = infixOp bop x y
    fmt (ERel rop x y) = infixOp rop x y
    fmt (ENum nop x y) = infixOp nop x y
    fmt (ECond p t e)  = cond p t e
    fmt (ECase p ms)   = empty

instance Format Pattern where
    fmt _ = text "pattern"

instance Format BinOp where
    fmt And = text "and"
    fmt Or  = text "or"

instance Format RelOp where
    fmt Greater   = char '>'
    fmt GreaterEq = text ">="
    fmt Less      = char '<'
    fmt LessEq    = text "<="

instance Format NumOp where
    fmt Add      = char '+'
    fmt Subtract = char '-'
    fmt Multiply = char '*'
    fmt Divide   = char '/'

cond :: (Format a, Format b) => a -> b -> b -> Doc
cond p t e = nest 2 $ text "if" <+> fmt p <$> b "then" t <$> b "else" e
  where
    b w = (text w <+>) . nest 2 . fmt

infixOp :: (Format a, Format b) => a -> b -> b -> Doc
infixOp op x y = fmt x <+> fmt op <+> fmt y

binding :: Format a => Int -> Ident -> Doc -> a -> Doc
binding n i sep' f = fill n (ident i) <+> sep' <+> fmt f

ident :: Ident -> Doc
ident = text . T.pack

vfmt :: Format a => [a] -> Doc
vfmt = vsep . map fmt

block :: Doc -> Doc
block = (<> line) . nest 2
