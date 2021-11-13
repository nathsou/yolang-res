open Belt

type lexer<'a> = Slice.t<char> => option<('a, Slice.t<char>)>

let sat = (pred): lexer<char> => {
  input => {
    switch input->Slice.head {
    | Some(c) if pred(c) => Some((c, input->Slice.step))
    | _ => None
    }
  }
}

let char = c => sat(c2 => c2 == c)

let isLowerCaseLetter = c => c >= 'a' && c <= 'z'
let isUpperCaseLetter = c => c >= 'A' && c <= 'Z'
let isAlpha = c => isLowerCaseLetter(c) || isUpperCaseLetter(c)
let isDigit = c => c >= '0' && c <= '9'
let isAlphaNum = c => isAlpha(c) || isDigit(c)

let lowerLetter = sat(isLowerCaseLetter)
let upperLetter = sat(isUpperCaseLetter)
let letter = sat(isAlpha)
let digit = sat(isDigit)
let alphaNum = sat(isAlphaNum)

let isSpace = char =>
  switch char {
  | ' ' => true
  | '\t' => true
  | '\n' => true
  | '\r' => true
  | _ => false
  }

let space = sat(isSpace)

let map = (p: lexer<'a>, f: 'a => 'b): lexer<'b> => {
  input => p(input)->Option.map(((a, rem)) => (f(a), rem))
}

let rec many = (~acc=[], lex: lexer<'a>): lexer<array<'a>> => {
  input => {
    if Slice.isEmpty(input) {
      Some((acc, input))
    } else {
      switch lex(input) {
      | Some((a, remaining)) => {
          let _ = acc->Js.Array2.push(a)
          many(lex, ~acc)(remaining)
        }
      | None => Some(acc, input)
      }
    }
  }
}

let spaces = many(space)

let then = (pa: lexer<'a>, pb: lexer<'b>): lexer<('a, 'b)> => {
  input =>
    pa(input)->Option.flatMap(((a, rema)) => pb(rema)->Option.map(((b, remb)) => ((a, b), remb)))
}

let some = (parser: lexer<'a>): lexer<array<'a>> => {
  then(parser, many(parser))->map(((a, bs)) => Array.concat([a], bs))
}

let seq3 = (a: lexer<'a>, b: lexer<'b>, c: lexer<'c>): lexer<('a, 'b, 'c)> => {
  input =>
    then(a, b)(input)->Option.flatMap((((a, b), rem)) =>
      c(rem)->Option.map(((c, rem)) => ((a, b, c), rem))
    )
}

let rec seqList = (ls: list<lexer<'a>>): lexer<list<'a>> => {
  switch ls {
  | list{} => input => Some((list{}, input))
  | list{l, ...ps} => then(l, seqList(ps))->map(((h, tl)) => list{h, ...tl})
  }
}

let seq = (ls: array<lexer<'a>>) => seqList(ls->List.fromArray)

let alt = (la: lexer<'a>, lb: lexer<'a>): lexer<'a> => {
  input =>
    switch la(input) {
    | Some((a, rem)) => Some((a, rem))
    | None => lb(input)
    }
}

let anyOf = (ls: array<lexer<'a>>): lexer<'a> => {
  input => ls->Utils.Array.firstSomeBy(lexer => lexer(input))
}

let string = str => {
  seq(str->Js.String.castToArrayLike->Js.Array.fromMap(c => char(c->String.get(0))))->map(_ => str)
}

let different = (l: lexer<'a>): lexer<'a> => {
  input =>
    switch l(input) {
    | Some(_) => None
    | None => Some((input->Slice.head->Option.getExn, input->Slice.tail))
    }
}
