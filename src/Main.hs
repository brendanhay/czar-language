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

import Czar.Language.Interpreter
import System.Console.Readline

main :: IO ()
main = do
    putStrLn "Starting czar in interpreted mode..."
    repl

repl :: IO ()
repl = do
    mline <- readline "> "
    case mline of
        Nothing   -> return ()
        Just ":q" -> return ()
        Just line -> do
            addHistory line
            print $ parseManifest line
            repl
