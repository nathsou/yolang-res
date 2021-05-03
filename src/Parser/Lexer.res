open LexerCombinators
open Token
open Belt

let int =
  some(digit)->map(digits => Nat(
    Int.fromString(digits->Array.joinWith("", c => String.make(1, c)))->Option.getExn,
  ))

let bool = alt(string("true")->map(_ => Bool(true)), string("false")->map(_ => Bool(false)))

// [a-z_][a-zA-Z0-9_\']*
let identifier =
  alt(lowerLetter, char('_'))
  ->then(many(anyOf([aphaNum, char('_'), char('\'')])))
  ->map(((h, tl)) => Identifier(
    String.make(1, h) ++ tl->Array.joinWith("", c => String.make(1, c)),
  ))

let keyword = anyOf([
  string("let")->map(_ => Keyword(Keywords.Let)),
  string("mut")->map(_ => Keyword(Keywords.Mut)),
  string("in")->map(_ => Keyword(Keywords.In)),
  string("if")->map(_ => Keyword(Keywords.If)),
  string("else")->map(_ => Keyword(Keywords.Else)),
  string("fn")->map(_ => Keyword(Keywords.Fn)),
  string("while")->map(_ => Keyword(Keywords.While)),
  string("return")->map(_ => Keyword(Keywords.Return)),
])

let symbol = anyOf([
  char('(')->map(_ => Symbol(Lparen)),
  char(')')->map(_ => Symbol(Rparen)),
  char(',')->map(_ => Symbol(Comma)),
  char(';')->map(_ => Symbol(SemiColon)),
  string("==")->map(_ => BinaryOp(Eq)),
  string("!=")->map(_ => BinaryOp(Neq)),
  char('=')->map(_ => Symbol(EqualSign)),
  char('{')->map(_ => Symbol(Lbracket)),
  char('}')->map(_ => Symbol(Rbracket)),
  string("->")->map(_ => Symbol(RightArrow)),
  char('+')->map(_ => BinaryOp(Plus)),
  char('-')->map(_ => BinaryOp(Sub)),
  char('*')->map(_ => BinaryOp(Mult)),
  char('/')->map(_ => BinaryOp(Div)),
  char('%')->map(_ => BinaryOp(Mod)),
  string("<=")->map(_ => BinaryOp(Leq)),
  string(">=")->map(_ => BinaryOp(Geq)),
  char('<')->map(_ => BinaryOp(Lss)),
  char('>')->map(_ => BinaryOp(Gtr)),
])

let token = anyOf([int, bool, keyword, symbol, identifier])

let lex = many(seq3(spaces, token, spaces)->map(((_, token, _)) => token))
