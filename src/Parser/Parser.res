open Combinators
open Expr
open Belt
open Token

// precedence from highest to lowest
let expr: parser<Expr.t> = ref(_ => None)
let block: parser<Expr.t> = ref(_ => None)

let int = satBy(t =>
  switch t {
  | Nat(n) => Some(ConstExpr(IntConst(n)))
  | _ => None
  }
)

let bool = satBy(t =>
  switch t {
  | Bool(b) => Some(ConstExpr(BoolConst(b)))
  | _ => None
  }
)

let ident = satBy(t =>
  switch t {
  | Identifier(n) => Some(n)
  | _ => None
  }
)

let var = ident->map(x => VarExpr(x))

let unit = then(token(Symbol(Lparen)), token(Symbol(Rparen)))->map(_ => ConstExpr(UnitConst))

let primary = anyOf([int, bool, unit, var, parens(expr)])

let app = alt(
  then(primary, some(parens(commas(expr))))->map(((f, args)) =>
    args->Array.reduce(f, (f, args) => AppExpr(f, args))
  ),
  primary,
)

let addOp = alt(
  token(BinaryOp(BinOp.Plus))->map(_ => BinOp.Plus),
  token(BinaryOp(BinOp.Sub))->map(_ => BinOp.Sub),
)

let multOp = anyOf([
  token(BinaryOp(BinOp.Mult))->map(_ => BinOp.Mult),
  token(BinaryOp(BinOp.Div))->map(_ => BinOp.Div),
  token(BinaryOp(BinOp.Mod))->map(_ => BinOp.Mod),
])

let comparisonOp = anyOf([
  token(BinaryOp(BinOp.Lss))->map(_ => BinOp.Lss),
  token(BinaryOp(BinOp.Leq))->map(_ => BinOp.Leq),
  token(BinaryOp(BinOp.Gtr))->map(_ => BinOp.Gtr),
  token(BinaryOp(BinOp.Geq))->map(_ => BinOp.Geq),
])

let eqOp = alt(
  token(BinaryOp(BinOp.Eq))->map(_ => BinOp.Eq),
  token(BinaryOp(BinOp.Neq))->map(_ => BinOp.Neq),
)

let factor = app

let term = chainLeft(factor, multOp, (a, op, b) => BinOpExpr(a, op, b))

let arith = chainLeft(term, addOp, (a, op, b) => BinOpExpr(a, op, b))

let comparison = chainLeft(arith, comparisonOp, (a, op, b) => BinOpExpr(a, op, b))

let equality = chainLeft(comparison, eqOp, (a, op, b) => BinOpExpr(a, op, b))

let ifThenElse = alt(
  seq5(token(Keyword(Keywords.If)), expr, block, token(Keyword(Keywords.Else)), block)->map(((
    _if,
    cond,
    thenExpr,
    _else,
    elseExpr,
  )) => IfExpr(cond, thenExpr, elseExpr)),
  equality,
)

let assignment = alt(
  seq4(token(Keyword(Keywords.Let)), ident, token(Symbol(EqualSign)), expr)->map(((
    _let,
    x,
    _eq,
    e,
  )) => LetExpr(x, e)),
  ifThenElse,
)

let letIn = alt(
  seq6(
    token(Keyword(Keywords.Let)),
    ident,
    token(Symbol(EqualSign)),
    expr,
    token(Keyword(Keywords.In)),
    expr,
  )->map(((_, x, _, e1, _, e2)) => LetInExpr(x, e1, e2)),
  assignment,
)

let arguments = alt(parens(commas(ident)), ident->map(x => [x]))

let lambda = alt(
  seq3(arguments, token(Symbol(RightArrow)), expr)->map(((args, _, body)) => FuncExpr(args, body)),
  letIn,
)

block :=
  alt(
    seq3(token(Symbol(Lbracket)), semiColons(expr), token(Symbol(Rbracket)))->map(((
      _,
      exprs,
      _,
    )) => BlockExpr(exprs->List.fromArray)),
    lambda,
  ).contents

expr := block.contents

let parse = str => {
  // wrap the whole program inside a top-level block
  Lexer.lex(Slice.fromString(`{${str}}`))->Option.flatMap(((tokens, _)) => {
    expr.contents(Slice.make(tokens))
  })
}
