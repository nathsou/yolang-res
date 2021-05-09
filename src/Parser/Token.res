module Symbol = {
  type t =
    | Lparen
    | Rparen
    | Comma
    | SemiColon
    | Eq
    | RightArrow
    | Lbracket
    | Rbracket
    | Colon
    | Plus
    | Minus
    | Star
    | Div
    | Percent
    | EqEq
    | Neq
    | Lss
    | Leq
    | Gtr
    | Geq
    | Bang

  let show = s =>
    switch s {
    | Lparen => "("
    | Rparen => ")"
    | Comma => ","
    | SemiColon => ";"
    | Eq => "="
    | RightArrow => "->"
    | Lbracket => "{"
    | Rbracket => "}"
    | Colon => ":"
    | Plus => "+"
    | Minus => "-"
    | Star => "*"
    | Div => "/"
    | Percent => "%"
    | EqEq => "=="
    | Neq => "!="
    | Lss => "<"
    | Leq => "<="
    | Gtr => ">"
    | Geq => ">="
    | Bang => "!"
    }
}

module Keywords = {
  type t = Let | Mut | In | If | Else | Fn | While | Return | As

  let show = kw =>
    switch kw {
    | Let => "let"
    | Mut => "mut"
    | In => "in"
    | If => "if"
    | Else => "else"
    | Fn => "fn"
    | While => "while"
    | Return => "return"
    | As => "as"
    }
}

type t =
  | Nat(int)
  | Bool(bool)
  | Symbol(Symbol.t)
  | Identifier(string)
  | UppercaseIdentifier(string)
  | Keyword(Keywords.t)

let show = token =>
  switch token {
  | Nat(n) => Belt.Int.toString(n)
  | Bool(b) => b ? "true" : "false"
  | Symbol(s) => Symbol.show(s)
  | Identifier(name) => name
  | UppercaseIdentifier(name) => name
  | Keyword(kw) => kw->Keywords.show
  }

let debug = token => {
  let typ = switch token {
  | Nat(_) => "nat"
  | Bool(_) => "bool"
  | Symbol(_) => "symbol"
  | Identifier(_) => "identifier"
  | UppercaseIdentifier(_) => "upperIdent"
  | Keyword(_) => "keyword"
  }

  `<${typ}: ${show(token)}>`
}
