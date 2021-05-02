open Belt
open Token

type parser<'a> = ref<Slice.t<Token.t> => option<('a, Slice.t<Token.t>)>>

let sat = (pred): parser<Token.t> => {
  ref(tokens =>
    tokens->Slice.head->Option.flatMap(h => pred(h) ? Some((h, tokens->Slice.tail)) : None)
  )
}

let satBy = (f: Token.t => option<'a>): parser<'a> => {
  ref(tokens =>
    tokens->Slice.head->Option.flatMap(h => f(h)->Option.map(a => (a, tokens->Slice.tail)))
  )
}

let token = (t: Token.t) => sat(t2 => t2 == t)

let map = (p: parser<'a>, f: 'a => 'b): parser<'b> => {
  ref(input => p.contents(input)->Option.map(((a, rem)) => (f(a), rem)))
}

let then = (pa: parser<'a>, pb: parser<'b>): parser<('a, 'b)> => {
  ref(input =>
    pa.contents(input)->Option.flatMap(((a, rema)) =>
      pb.contents(rema)->Option.map(((b, remb)) => ((a, b), remb))
    )
  )
}

let many = (parser: parser<'a>): parser<array<'a>> => {
  let rec aux = (parser: parser<'a>): parser<list<'a>> => {
    ref(tokens => {
      if tokens->Slice.isEmpty {
        Some((list{}, tokens))
      } else {
        switch parser.contents(tokens) {
        | Some((a, remaining)) =>
          aux(parser).contents(remaining)->Option.map(((tail, rem)) => (list{a, ...tail}, rem))
        | None => Some(list{}, tokens)
        }
      }
    })
  }

  aux(parser)->map(lst => lst->List.toArray)
}

let some = (parser: parser<'a>): parser<array<'a>> => {
  then(parser, many(parser))->map(((a, bs)) => Array.concat([a], bs))
}

let seq3 = (a: parser<'a>, b: parser<'b>, c: parser<'c>): parser<('a, 'b, 'c)> => {
  ref(input =>
    then(a, b).contents(input)->Option.flatMap((((a, b), rem)) =>
      c.contents(rem)->Option.map(((c, rem)) => ((a, b, c), rem))
    )
  )
}

let seq4 = (a: parser<'a>, b: parser<'b>, c: parser<'c>, d: parser<'d>): parser<(
  'a,
  'b,
  'c,
  'd,
)> => {
  ref(input =>
    seq3(a, b, c).contents(input)->Option.flatMap((((a, b, c), rem)) =>
      d.contents(rem)->Option.map(((d, rem)) => ((a, b, c, d), rem))
    )
  )
}

let seq5 = (a: parser<'a>, b: parser<'b>, c: parser<'c>, d: parser<'d>, e: parser<'e>): parser<(
  'a,
  'b,
  'c,
  'd,
  'e,
)> => {
  ref(input =>
    seq4(a, b, c, d).contents(input)->Option.flatMap((((a, b, c, d), rem)) =>
      e.contents(rem)->Option.map(((e, rem)) => ((a, b, c, d, e), rem))
    )
  )
}

let seq6 = (
  a: parser<'a>,
  b: parser<'b>,
  c: parser<'c>,
  d: parser<'d>,
  e: parser<'e>,
  f: parser<'f>,
): parser<('a, 'b, 'c, 'd, 'e, 'f)> => {
  ref(input =>
    seq5(a, b, c, d, e).contents(input)->Option.flatMap((((a, b, c, d, e), rem)) =>
      f.contents(rem)->Option.map(((f, rem)) => ((a, b, c, d, e, f), rem))
    )
  )
}

let rec seq = (ps: list<parser<'a>>): parser<list<'a>> => {
  switch ps {
  | list{} => ref(input => Some((list{}, input)))
  | list{p, ...ps} => then(p, seq(ps))->map(((h, tl)) => list{h, ...tl})
  }
}

let alt = (pa: parser<'a>, pb: parser<'a>): parser<'a> => {
  ref(input =>
    switch pa.contents(input) {
    | Some((a, rem)) => Some((a, rem))
    | None => pb.contents(input)
    }
  )
}

let anyOf = (ps: array<parser<'a>>): parser<'a> => {
  ref(tokens => ps->ArrayUtils.firstSomeBy(p => p.contents(tokens)))
}

let leftAssoc = (left: parser<'a>, right: parser<'b>, combine: ('a, 'b) => 'c): parser<'c> => {
  then(left, many(right))->map(((h, tl)) => tl == [] ? h : tl->Array.reduce(h, combine))
}

let chainLeft = (p: parser<'a>, op: parser<'b>, combine: ('a, 'b, 'a) => 'c): parser<'c> => {
  leftAssoc(p, then(op, p), (a, (op, b)) => combine(a, op, b))
}

let parens = p => seq3(token(Symbol(Lparen)), p, token(Symbol(Rparen)))->map(((_, a, _)) => a)

let sepBy = (p, sep) => {
  let rec aux = (tokens, acc) => {
    switch p.contents(tokens) {
    | None => (acc, tokens)
    | Some((a, tokens)) => {
        let _ = Js.Array.push(a, acc)
        sep.contents(tokens)->Option.mapWithDefault((acc, tokens), ((_, remTokens)) =>
          aux(remTokens, acc)
        )
      }
    }
  }

  ref(tokens => Some(aux(tokens, [])))
}

let commas = p => sepBy(p, token(Symbol(Comma)))
let semicolons = p => sepBy(p, token(Symbol(SemiColon)))
