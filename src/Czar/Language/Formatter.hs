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
    fmt (QName is) = text . T.pack $ intercalate "." is

instance Format RName where
    fmt (RName q i) = fmt q <$> text (T.pack i)

instance Format Manifest where
    fmt (Manifest q bs ds es) = m <$> vfmt ds <$> vfmt es
      where
        m = block $ text "manifiest" <+> fmt q <$> fmt bs

instance Format Decl where
    fmt (Decl i q bs) = block $ text (T.pack i) <+> fmt q <$> fmt bs

instance Format [Bind] where
    fmt [] = empty
    fmt bs = nest 2 . (text "where" <$>) . vfmt $ zip (repeat m) bs
      where
        m = foldl max 0 $ map (length . bindIdent) bs

instance Format (Int, Bind) where
    fmt (n, AExp i exp)   = binding n i "=" exp
    fmt (n, ARef i rname) = binding n i "=" rname
    fmt (n, ASig i typ)   = binding n i "::" typ

instance Format Type where
    fmt TInt        = text "Int"
    fmt TFloat      = text "Float"
    fmt TChar       = text "Char"
    fmt TString     = text "String"
    fmt TBool       = text "Bool"
    fmt (TList typ) = list [fmt typ]
    fmt (TTuple ts) = tupled $ map fmt ts

instance Format Exp where
    fmt _ = text "exp"

vfmt :: Format a => [a] -> Doc
vfmt = vsep . map fmt

binding :: Format a => Int -> Ident -> Text -> a -> Doc
binding n ident sep f = fill n (text $ T.pack ident) <+> text sep <+> fmt f

block :: Doc -> Doc
block = (<> line) . nest 2

infix 4 <?>
(<?>) :: Maybe Doc -> (Doc -> Doc)
(<?>) = maybe id (<+>)
