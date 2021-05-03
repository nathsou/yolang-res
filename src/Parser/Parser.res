open Combinators
open Ast
open Belt
open Token

let decl: parser<Decl.t> = ref(_ => None)
let expr: parser<Expr.t> = ref(_ => None)
let block: parser<Expr.t> = ref(_ => None)
let stmt: parser<Stmt.t> = ref(_ => None)

// expressions

// precedence from highest to lowest

let int = satBy(t =>
  switch t {
  | Nat(n) => Some(Ast.ConstExpr(U32Const(n)))
  | _ => None
  }
)

let bool = satBy(t =>
  switch t {
  | Bool(b) => Some(Ast.ConstExpr(BoolConst(b)))
  | _ => None
  }
)

let ident = satBy(t =>
  switch t {
  | Identifier(n) => Some(n)
  | _ => None
  }
)

let var = ident->map(x => Ast.VarExpr(x))

let unit = then(token(Symbol(Lparen)), token(Symbol(Rparen)))->map(_ => Ast.ConstExpr(UnitConst))

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
  seq4(token(Keyword(Keywords.If)), expr, block, optional(then(token(Keyword(Keywords.Else)), block)))->map(((
    _if,
    cond,
    thenExpr,
    elseBranch
  )) => Ast.IfExpr(cond, thenExpr, elseBranch->Option.map(((_, elseExpr)) => elseExpr))),
  equality,
)

let whileExpr = alt(
  seq3(token(Keyword(Keywords.While)), expr, block)->map(((_, cond, body)) => Ast.WhileExpr(cond, body)),
  ifThenElse
)

let letIn = alt(
  seq6(
    token(Keyword(Keywords.Let)),
    ident,
    token(Symbol(EqualSign)),
    expr,
    token(Keyword(Keywords.In)),
    expr,
  )->map(((_, x, _, e1, _, e2)) => Ast.LetInExpr(x, e1, e2)),
  whileExpr,
)

let arguments = alt(parens(commas(ident)), ident->map(x => [x]))

let lambda = alt(
  seq3(arguments, token(Symbol(RightArrow)), expr)->map(((args, _, body)) => Ast.FuncExpr(
    args,
    body,
  )),
  letIn,
)

let assignment = alt(
  seq3(ident, token(Symbol(Symbol.EqualSign)), expr)->map(((x, _, val)) => Ast.AssignmentExpr(
    x,
    val,
  )),
  lambda,
)

let implicitStms = keepBy(expr, expr => switch expr {
  | Ast.IfExpr(_, _, _) => Some((true, Ast.ExprStmt(expr)))
  | Ast.WhileExpr(_, _) => Some((true, Ast.ExprStmt(expr)))
  | _ => None
})

block :=
  alt(
    seq4(
      token(Symbol(Lbracket)),
      many(alt(stmt->map(s => (false, s)), implicitStms)),
      optional(expr),
      token(Symbol(Rbracket)),
    )->map(((_, stmts, lastExpr, _)) => {
      let ss = stmts->Array.map(((_, s)) => s)
      switch stmts->Array.get(stmts->Array.length - 1) {
        | Some((isImplicit, Ast.ExprStmt(expr))) if isImplicit && lastExpr->Option.isNone => {
          Ast.BlockExpr(ss->Array.slice(~offset=0, ~len=(stmts->Array.length - 2)), Some(expr))
        }
        | _ => Ast.BlockExpr(ss, lastExpr)
      }
    }
  ), assignment).contents

let returnExpr = alt(then(token(Keyword(Keywords.Return)), expr)->map(((_, ret)) => Ast.ReturnExpr(ret)), block)

expr := returnExpr.contents

// statements

let exprStmt = then(expr, token(Symbol(Symbol.SemiColon)))->map(((expr, _)) => Ast.ExprStmt(expr))

let letStmt = alt(
  seq6(
    token(Keyword(Keywords.Let)),
    optional(token(Keyword(Keywords.Mut))),
    ident,
    token(Symbol(EqualSign)),
    expr,
    token(Symbol(Symbol.SemiColon)),
  )->map(((_let, mut, x, _eq, e, _)) => Ast.LetStmt(x, mut->Option.isSome, e)),
  exprStmt,
)

stmt := letStmt.contents

// declarations

let funDecl =
  seq4(token(Keyword(Keywords.Fn)), ident, parens(commas(ident)), block)->map(((
    _,
    f,
    args,
    body,
  )) => Ast.FuncDecl(f, args, body))

decl := funDecl.contents

let prog = many(decl)

let parse = input => {
  Lexer.lex(Slice.fromString(input))->Option.flatMap(((tokens, _)) => {
    prog.contents(Slice.make(tokens))
  })
}
