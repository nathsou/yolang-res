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
  ->then(many(anyOf([alphaNum, char('_'), char('\'')])))
  ->map(((h, tl)) => Identifier(
    String.make(1, h) ++ tl->Array.joinWith("", c => String.make(1, c)),
  ))

let uppercaseIdentifier =
  many(anyOf([alphaNum, char('_')]))->map(chars => UppercaseIdentifier(
    chars->Array.joinWith("", c => String.make(1, c)),
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
  string("as")->map(_ => Keyword(Keywords.As)),
  string("unsafe")->map(_ => Keyword(Keywords.Unsafe)),
])

let symbol = anyOf([
  string("->")->map(_ => Symbol(RightArrow)),
  string("==")->map(_ => Symbol(EqEq)),
  string("!=")->map(_ => Symbol(Neq)),
  char('+')->map(_ => Symbol(Plus)),
  char('-')->map(_ => Symbol(Minus)),
  char('*')->map(_ => Symbol(Star)),
  char('/')->map(_ => Symbol(Div)),
  char('%')->map(_ => Symbol(Percent)),
  string("<=")->map(_ => Symbol(Leq)),
  string(">=")->map(_ => Symbol(Geq)),
  char('<')->map(_ => Symbol(Lss)),
  char('>')->map(_ => Symbol(Gtr)),
  char('(')->map(_ => Symbol(Lparen)),
  char(')')->map(_ => Symbol(Rparen)),
  char(',')->map(_ => Symbol(Comma)),
  char(';')->map(_ => Symbol(SemiColon)),
  char('=')->map(_ => Symbol(Eq)),
  char('{')->map(_ => Symbol(Lbracket)),
  char('}')->map(_ => Symbol(Rbracket)),
  char(':')->map(_ => Symbol(Colon)),
  char('!')->map(_ => Symbol(Bang)),
])

let token = anyOf([int, bool, keyword, symbol, identifier, uppercaseIdentifier])

let lex = many(seq3(spaces, token, spaces)->map(((_, token, _)) => token))
