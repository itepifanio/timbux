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
// Rodando pelo ghci
alex ./src/lexer.x && ghci ./src/parser.hs ./src/token.hs ./src/lexer.hs ./src/statements.hs ./src/memory.hs
main
```

