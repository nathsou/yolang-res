open Belt

type t = LetDecl(string, Expr.t) | FuncDecl(string, array<string>, Expr.t)

let show = decl =>
  switch decl {
  | LetDecl(x, val) => `let ${x} = ${Expr.show(val)}`
  | FuncDecl(f, args, body) => `fn ${f}(${args->Array.joinWith(", ", x => x)}) ${Expr.show(body)}`
  }
