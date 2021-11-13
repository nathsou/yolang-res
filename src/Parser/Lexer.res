open LexerCombinators
open Token
open Belt

let digits =
  some(digit)->map(digits =>
    Int.fromString(digits->Array.joinWith("", c => String.make(1, c)))->Option.getExn
  )

let u8 = digits->then(string("u8"))->map(((n, _)) => Nat(n, #8))
let u32 = alt(
  digits->then(string("u32"))->map(((n, _)) => Nat(n, #32)),
  digits->map(n => Nat(n, #32)),
)

let integer = anyOf([u8, u32])

let boolean = alt(string("true")->map(_ => Bool(true)), string("false")->map(_ => Bool(false)))

// [a-z_][a-zA-Z0-9_\']*
let identifier =
  alt(lowerLetter, char('_'))
  ->then(many(anyOf([alphaNum, char('_'), char('\'')])))
  ->map(((h, tl)) => Identifier(
    String.make(1, h) ++ tl->Array.joinWith("", c => String.make(1, c)),
  ))

let stringOfChars = chars => chars->Array.joinWith("", c => String.make(1, c))

let uppercaseIdentifier =
  some(anyOf([alphaNum, char('_')]))->map(chars => UppercaseIdentifier(stringOfChars(chars)))

let character = seq3(char('\''), different(char('\'')), char('\''))->map(((_, c, _)) => {
  let code = Char.code(c)
  if code > 0x7f {
    raise(ParserExn.InvalidCharacter(c))
  }

  Char(c)
})

let str =
  seq3(char('"'), many(different(char('"'))), char('"'))->map(((_, s, _)) => String(
    stringOfChars(s),
  ))

let keyword =
  then(
    anyOf([
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
      string("struct")->map(_ => Keyword(Keywords.Struct)),
      string("impl")->map(_ => Keyword(Keywords.Impl)),
      string("extern")->map(_ => Keyword(Keywords.Extern)),
    ]),
    different(alphaNum),
  )->map(((kw, _)) => kw)

let symbol = anyOf([
  string("->")->map(_ => Symbol(RightArrow)),
  string("==")->map(_ => Symbol(EqEq)),
  string("!=")->map(_ => Symbol(Neq)),
  string("&&")->map(_ => Symbol(DoubleAmpersand)),
  string("||")->map(_ => Symbol(DoublePipe)),
  string("+=")->map(_ => Symbol(PlusEq)),
  string("-=")->map(_ => Symbol(MinusEq)),
  string("*=")->map(_ => Symbol(StarEq)),
  string("/=")->map(_ => Symbol(DivEq)),
  string("%=")->map(_ => Symbol(ModEq)),
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
  char('[')->map(_ => Symbol(LSqBracket)),
  char(']')->map(_ => Symbol(RSqBracket)),
  char(':')->map(_ => Symbol(Colon)),
  char('!')->map(_ => Symbol(Bang)),
  char('.')->map(_ => Symbol(Dot)),
  char('&')->map(_ => Symbol(Ampersand)),
  char('|')->map(_ => Symbol(Pipe)),
  char('\'')->map(_ => Symbol(SingleQuote)),
  char('"')->map(_ => Symbol(DoubleQuote)),
])

let token = anyOf([
  integer,
  boolean,
  character,
  str,
  keyword,
  symbol,
  identifier,
  uppercaseIdentifier,
])

let lex = many(seq3(spaces, token, spaces)->map(((_, token, _)) => token))
