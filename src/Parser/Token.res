module BinOp = {
  type t = Plus | Sub | Mult | Div | Mod | EqEq | Neq | Lss | Leq | Gtr | Geq

  let show = op =>
    switch op {
    | Plus => "+"
    | Sub => "-"
    | Mult => "*"
    | Div => "/"
    | Mod => "%"
    | EqEq => "=="
    | Neq => "!="
    | Lss => "<"
    | Leq => "<="
    | Gtr => ">"
    | Geq => ">="
    }
}

module Symbol = {
  type t = Lparen | Rparen | Comma | SemiColon | Eq | RightArrow | Lbracket | Rbracket

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
    }
}

module Keywords = {
  type t = Let | Mut | In | If | Else | Fn | While | Return

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
    }
}

type t =
  | Nat(int)
  | Bool(bool)
  | BinaryOp(BinOp.t)
  | Symbol(Symbol.t)
  | Identifier(string)
  | UppercaseIdentifier(string)
  | Keyword(Keywords.t)

let show = token =>
  switch token {
  | Nat(n) => Belt.Int.toString(n)
  | Bool(b) => b ? "true" : "false"
  | BinaryOp(op) => op->BinOp.show
  | Symbol(s) => Symbol.show(s)
  | Identifier(name) => name
  | UppercaseIdentifier(name) => name
  | Keyword(kw) => kw->Keywords.show
  }

let debug = token => {
  let typ = switch token {
  | Nat(_) => "nat"
  | Bool(_) => "bool"
  | BinaryOp(_) => "binop"
  | Symbol(_) => "symbol"
  | Identifier(_) => "identifier"
  | UppercaseIdentifier(_) => "uppercase identifier"
  | Keyword(_) => "keyword"
  }

  `<${typ}: ${show(token)}>`
}
