
// same module system as OCaml and ReScript

fn sayHello(name) {
  "Hello ${name}"
}

// in this case, the dot notation can be applied to
// any function taking a string as its first argument
"Nicolas".sayHello.print

struct Context {
  index: int
}

type List<A> {
  Cons(A, List<A>)
  Nil
}

fn nextIndex(ctx: mut<Context>) {
  ctx.index := ctx.index + 1
}