open Combinators
open Ast
open Belt
open Token

let decl: parser<Decl.t> = ref(_ => None)
let expr: parser<Expr.t> = ref(_ => None)
let block: parser<Expr.t> = ref(_ => None)
let stmt: parser<Stmt.t> = ref(_ => None)
let type_: parser<Types.monoTy> = ref(_ => None)

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

let uppercaseIdent = satBy(t =>
  switch t {
  | UppercaseIdentifier(n) => Some(n)
  | _ => None
  }
)

let globalName = satBy(t =>
  switch t {
  | UppercaseIdentifier(n) => Some(n)
  | Identifier(n) => Some(n)
  | _ => None
  }
)

let unitType = then(token(Symbol(Lparen)), token(Symbol(Rparen)))->map(_ => Types.unitTy)

let primitiveType = alt(
  unitType,
  satBy(t =>
    switch t {
    | Identifier("u32") => Some(Types.u32Ty)
    | Identifier("bool") => Some(Types.boolTy)
    | UppercaseIdentifier(name) => Some(Types.TyStruct(Types.NamedStruct(name)))
    | _ => None
    }
  ),
)

let primitiveTypeOrParens = alt(primitiveType, parens(type_))

let funcType = alt(
  seq3(
    alt(parens(commas(type_)), primitiveTypeOrParens->map(ty => [ty])),
    token(Symbol(RightArrow)),
    type_,
  )->map(((args, _, ret)) => Types.funTy(args, ret)),
  primitiveTypeOrParens,
)

let compoundType = alt(
  seq4(uppercaseIdent, token(Symbol(Lss)), commas(type_), token(Symbol(Gtr)))->map(((
    name,
    _,
    params,
    _,
  )) => Types.TyConst(name, params)),
  funcType,
)

type_ := compoundType.contents

let globalAnnotation =
  then(globalName, optional(then(token(Symbol(Colon)), type_)))->map(((x, ann)) => (
    x,
    ann->Option.map(((_, ty)) => ty),
  ))

let var = alt(ident, globalName)->map(x => Ast.VarExpr(x))
// let term = chainLeft(unary, multOp, (a, op, b) => BinOpExpr(a, op, b))

let unit = then(token(Symbol(Lparen)), token(Symbol(Rparen)))->map(_ => Ast.ConstExpr(UnitConst))

let tuple = parens(
  seq4(
    expr,
    token(Symbol(Symbol.Comma)),
    expr,
    optional(then(token(Symbol(Symbol.Comma)), commas(expr))),
  ),
)->map(((e1, _, e2, es)) => {
  let exprs = Array.concat([e1, e2], es->Option.mapWithDefault([], ((_, es)) => es))
  Ast.TupleExpr(exprs)
})

let primary = anyOf([int, bool, unit, var, tuple, parens(expr)])

let attributeAccess = alt(
  leftAssoc(primary, then(token(Symbol(Symbol.Dot)), ident), (
    lhs,
    (_, attr),
  ) => Ast.AttributeAccessExpr(lhs, attr)),
  primary,
)

let app = alt(
  then(attributeAccess, some(parens(commas(expr))))->map(((f, args)) =>
    args->Array.reduce(f, (f, args) => AppExpr(f, args))
  ),
  attributeAccess,
)

let unaryOp = anyOf([
  token(Symbol(Symbol.Star))->map(_ => UnaryOp.Deref),
  token(Symbol(Symbol.Minus))->map(_ => UnaryOp.Neg),
  token(Symbol(Symbol.Bang))->map(_ => UnaryOp.Not),
])

let addOp = alt(
  token(Symbol(Symbol.Plus))->map(_ => BinOp.Plus),
  token(Symbol(Symbol.Minus))->map(_ => BinOp.Sub),
)

let multOp = anyOf([
  token(Symbol(Symbol.Star))->map(_ => BinOp.Mult),
  token(Symbol(Symbol.Div))->map(_ => BinOp.Div),
  token(Symbol(Symbol.Percent))->map(_ => BinOp.Mod),
])

let comparisonOp = anyOf([
  token(Symbol(Symbol.Lss))->map(_ => BinOp.Lss),
  token(Symbol(Symbol.Leq))->map(_ => BinOp.Leq),
  token(Symbol(Symbol.Gtr))->map(_ => BinOp.Gtr),
  token(Symbol(Symbol.Geq))->map(_ => BinOp.Geq),
])

let eqOp = alt(
  token(Symbol(Symbol.EqEq))->map(_ => BinOp.Equ),
  token(Symbol(Symbol.Neq))->map(_ => BinOp.Neq),
)

let factor = app

let unary = alt(then(unaryOp, factor)->map(((op, expr)) => Ast.UnaryOpExpr(op, expr)), factor)

let term = chainLeft(unary, multOp, (a, op, b) => BinOpExpr(a, op, b))

let arith = chainLeft(term, addOp, (a, op, b) => BinOpExpr(a, op, b))

let comparison = chainLeft(arith, comparisonOp, (a, op, b) => BinOpExpr(a, op, b))

let equality = chainLeft(comparison, eqOp, (a, op, b) => BinOpExpr(a, op, b))

let ifThenElse = alt(
  seq4(
    token(Keyword(Keywords.If)),
    expr,
    block,
    optional(then(token(Keyword(Keywords.Else)), block)),
  )->map(((_if, cond, thenExpr, elseBranch)) => Ast.IfExpr(
    cond,
    thenExpr,
    elseBranch->Option.map(((_, elseExpr)) => elseExpr),
  )),
  equality,
)

let whileExpr = alt(
  seq3(token(Keyword(Keywords.While)), expr, block)->map(((_, cond, body)) => Ast.WhileExpr(
    cond,
    body,
  )),
  ifThenElse,
)

let letIn = alt(
  seq6(
    token(Keyword(Keywords.Let)),
    ident,
    token(Symbol(Eq)),
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

let implicitStmts = keepBy(expr, expr =>
  switch expr {
  | Ast.IfExpr(_, _, _) => Some((true, Ast.ExprStmt(expr)))
  | Ast.WhileExpr(_, _) => Some((true, Ast.ExprStmt(expr)))
  | _ => None
  }
)

block :=
  alt(
    seq5(
      optional(token(Keyword(Keywords.Unsafe))),
      token(Symbol(Lbracket)),
      many(alt(stmt->map(s => (false, s)), implicitStmts)),
      optional(expr),
      token(Symbol(Rbracket)),
    )->map(((safety, _, stmts, lastExpr, _)) => {
      let ss = stmts->Array.map(((_, s)) => s)
      let safety = safety->Option.mapWithDefault(Safe, _ => Unsafe)
      switch stmts->Array.get(stmts->Array.length - 1) {
      | Some((isImplicit, Ast.ExprStmt(expr))) if isImplicit && lastExpr->Option.isNone =>
        Ast.BlockExpr(ss->Array.slice(~offset=0, ~len=stmts->Array.length - 2), Some(expr), safety)
      | _ => Ast.BlockExpr(ss, lastExpr, safety)
      }
    }),
    lambda,
  ).contents

let structExpr = alt(
  seq4(
    uppercaseIdent,
    token(Symbol(Symbol.Lbracket)),
    commas(seq3(ident, token(Symbol(Symbol.Colon)), expr)),
    token(Symbol(Symbol.Rbracket)),
  )->map(((name, _, entries, _)) => Ast.StructExpr(
    name,
    entries->Array.map(((attr, _, ty)) => (attr, ty)),
  )),
  block,
)

let assignment = chainLeft(structExpr, token(Symbol(Eq)), (a, _, b) => Ast.AssignmentExpr(a, b))

let returnExpr = alt(
  then(token(Keyword(Keywords.Return)), optional(expr))->map(((_, ret)) => Ast.ReturnExpr(ret)),
  assignment,
)

let typeAssertion = then(returnExpr, optional(then(token(Keyword(Keywords.As)), type_)))->map(((
  e,
  assertion,
)) =>
  switch assertion {
  | Some((_, ty)) => Ast.TypeAssertionExpr(e, ty)
  | None => e
  }
)

expr := typeAssertion.contents

// statements

let exprStmt = then(expr, token(Symbol(SemiColon)))->map(((expr, _)) => Ast.ExprStmt(expr))

let letStmt = alt(
  seq5(
    alt(token(Keyword(Keywords.Let)), token(Keyword(Keywords.Mut))),
    globalAnnotation,
    token(Symbol(Eq)),
    expr,
    token(Symbol(SemiColon)),
  )->map(((letOrMut, (x, ty), _eq, e, _)) => Ast.LetStmt(
    x,
    letOrMut == Keyword(Keywords.Mut),
    e,
    ty,
  )),
  exprStmt,
)

stmt := letStmt.contents

// declarations

let globalDecl =
  seq5(
    alt(token(Keyword(Keywords.Let)), token(Keyword(Keywords.Mut))),
    globalName,
    token(Symbol(Eq)),
    expr,
    token(Symbol(SemiColon)),
  )->map(((letOrMut, x, _eq, e, _)) => Ast.GlobalDecl(x, letOrMut == Keyword(Keywords.Mut), e))

let structDecl = alt(
  seq5(
    token(Keyword(Keywords.Struct)),
    uppercaseIdent,
    token(Symbol(Symbol.Lbracket)),
    commas(
      seq3(then(optional(token(Keyword(Keywords.Mut))), ident), token(Symbol(Symbol.Colon)), type_),
    ),
    token(Symbol(Symbol.Rbracket)),
  )->map(((_, name, _, entries, _)) => {
    let entries = entries->Array.map((((mut, attr), _, ty)) => (attr, ty, mut->Option.isSome))
    Ast.StructDecl(name, entries)
  }),
  globalDecl,
)

let funDecl = alt(
  seq4(
    token(Keyword(Keywords.Fn)),
    ident,
    parens(commas(then(optional(token(Keyword(Keywords.Mut))), ident)))->map(args =>
      args->Array.map(((mut, x)) => (x, mut->Option.isSome))
    ),
    block,
  )->map(((_, f, args, body)) => Ast.FuncDecl(f, args, body)),
  structDecl,
)

let externFunDecl = alt(
  seq7(
    token(Keyword(Keywords.Extern)),
    token(Keyword(Keywords.Fn)),
    ident,
    parens(
      commas(
        seq4(optional(token(Keyword(Keywords.Mut))), ident, token(Symbol(Symbol.Colon)), type_),
      )->map(args => args->Array.map(((mut, arg, _, ty)) => (arg, ty, mut->Option.isSome))),
    ),
    token(Symbol(Symbol.RightArrow)),
    type_,
    token(Symbol(SemiColon)),
  )->map(((_, _, f, args, _, ret, _)) => Ast.ExternFuncDecl({name: f, args: args, ret: ret})),
  funDecl,
)

let implDecl = alt(
  seq3(
    token(Keyword(Keywords.Impl)),
    uppercaseIdent,
    seq3(token(Symbol(Symbol.Lbracket)), many(funDecl), token(Symbol(Symbol.Rbracket)))->map(((
      _,
      decls,
      _,
    )) => decls),
  )->map(((_, structName, decls)) => Ast.ImplDecl(structName, decls)),
  externFunDecl,
)

decl := implDecl.contents

let prog = many(decl)

let parse = input => {
  let withoutComments = Js.String.replaceByRe(%re("/(\\/\\/[^\\n]*)/g"), "", input)
  switch Lexer.lex(Slice.fromString(withoutComments)) {
  | Some((tokens, _)) =>
    switch prog.contents(Slice.make(tokens)) {
    | Some((ast, rem)) =>
      if rem->Slice.isEmpty {
        Ok(ast)
      } else {
        let near =
          rem->Slice.toArray->Array.slice(~offset=0, ~len=5)->Array.joinWith(" ", Token.show)
        Error(`could not parse near ${near}`)
      }
    | _ => Error("could not parse")
    }
  | _ => Error("could not tokenize")
  }
}
