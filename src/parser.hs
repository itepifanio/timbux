module Parser where

import Typedata.Matrix
import Lexer
import Text.Parsec
import Control.Monad.IO.Class

import System.IO
import System.IO.Unsafe

getTokensAux file = do {fh <- openFile file ReadMode;
                      s <- hGetContents fh;
                      return (alexScanTokens s)}

getTokens file = unsafePerformIO (getTokensAux file)
