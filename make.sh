#rm *.hi
#rm *.o
#rm lexer.hs
alex ./src/lexer.x
ghc ./src/parser.hs ./src/lexer.hs ./src/token.hs ./src/statements.hs 
./src/parser
