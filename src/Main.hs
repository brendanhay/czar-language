-- |
-- Module      : Main
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Main where

import Control.Monad.Identity
import Czar.Language.Formatter
import Czar.Language.Parser
import System.Console.Readline
import System.IO
import Text.Parsec.IndentParsec

import qualified Data.Text.Lazy.IO as T

main :: IO ()
main = do
    f <- readFile "share/syntax.cz"
    either (\e -> putStrLn "Error:" >> print e)
           (\a -> do
                putStrLn "\nAST:"
                print a
                putStrLn "\nReified:"
                T.putStrLn $ render a)
           (parse f)

    putStrLn ""

    -- putStrLn "Starting czar in interpreted mode..."
    -- repl

repl :: IO ()
repl = do
    mline <- readline "> "
    case mline of
        Nothing   -> return ()
        Just ":q" -> return ()
        Just line -> do
            addHistory line
            print $ parse line
            repl

parse = runIdentity . runGIPT manifestParser () "parseManifest"
