rm ./src/*.hi
rm ./src/*.o
rm ./src/lexer.hs
alex ./src/lexer.x
ghc ./src/parser.hs ./src/lexer.hs ./src/token.hs ./src/statements.hs ./src/memory.hs ./src/evaluation.hs
./src/parser
