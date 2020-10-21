# Timbux

<p align="center">
  <img src="https://github.com/itepifanio/timbu/blob/master/docs/img/timbu.png">
</p>

Pronuncia-se "possum", Timbux é uma linguagem de script interpretada.

Usuários sobre a linguagem: "aaaaaaaaaaaaAAAAAAAAAAAAAaaAAAaa"

# Get it working

`alex ./src/lexer.x`<br><br>
`ghc ./src/lexer.hs ./src/token.hs ./src/statements.hs ./src/parser.hs`<br><br>
`./src/parser.hs`<br><br>

```shell
// Rodando tudo em uma linha
alex ./src/lexer.x && ghc ./src/lexer.hs && ./src/lexer < ./program/program.pe && ghci src/parser.hs src/lexer.hs src/typedata/matrix.hs
```

