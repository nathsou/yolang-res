open Belt
open Core
open Types
open Subst
open Unification

let constTy = (c: Ast.Const.t): polyTy => {
  let ty = switch c {
  | Ast.Const.U32Const(_) => u32Ty
  | Ast.Const.BoolConst(_) => boolTy
  | Ast.Const.UnitConst => unitTy
  }

  polyOf(ty)
}

let binOpTy = (op: Ast.BinOp.t): polyTy => {
  open Ast.BinOp
  switch op {
  | Plus => polyOf(funTy([u32Ty, u32Ty], u32Ty))
  | Sub => polyOf(funTy([u32Ty, u32Ty], u32Ty))
  | Mult => polyOf(funTy([u32Ty, u32Ty], u32Ty))
  | Div => polyOf(funTy([u32Ty, u32Ty], u32Ty))
  | Mod => polyOf(funTy([u32Ty, u32Ty], u32Ty))
  | Equ => ([0], funTy([TyVar(0), TyVar(0)], boolTy)) // (a, a) -> bool
  | Neq => ([0], funTy([TyVar(0), TyVar(0)], boolTy)) // (a, a) -> bool
  | Lss => polyOf(funTy([u32Ty, u32Ty], boolTy))
  | Leq => polyOf(funTy([u32Ty, u32Ty], boolTy))
  | Gtr => polyOf(funTy([u32Ty, u32Ty], boolTy))
  | Geq => polyOf(funTy([u32Ty, u32Ty], boolTy))
  }
}

let unaryOpTy = (op: Ast.UnaryOp.t): polyTy => {
  open Ast.UnaryOp
  switch op {
  | Neg => polyOf(funTy([u32Ty], u32Ty))
  | Not => polyOf(funTy([boolTy], boolTy))
  | Deref => ([0], funTy([pointerTy(TyVar(0))], TyVar(0))) // Ptr<a> -> a
  }
}

// rewrite
// { let a = 3; let b = 7; a * b }
// into
// let a = 3 in let b = 7 in a * b
let rec rewriteExpr = expr => {
  open CoreAst

  switch expr {
  | CoreBlockExpr(tau, stmts, lastExpr, safety) => {
      let rec aux = stmts =>
        switch stmts {
        | list{CoreLetStmt(x, _, e), ...tl} =>
          Some(
            CoreLetInExpr(
              tau,
              x,
              rewriteExpr(e),
              aux(tl)->Option.mapWithDefault(CoreConstExpr(Ast.Const.UnitConst), x => x),
            ),
          )
        | list{stmt, ...tl} => Some(CoreBlockExpr(tau, [rewriteStmt(stmt)], aux(tl), safety))
        | list{} => lastExpr->Option.map(rewriteExpr)
        }

      CoreBlockExpr(tau, [], aux(stmts->List.fromArray), safety)
    }
  | _ => expr
  }
}

and rewriteStmt = stmt =>
  switch stmt {
  | CoreExprStmt(expr) => expr->rewriteExpr->CoreExprStmt
  | CoreLetStmt(x, mut, rhs) => CoreLetStmt(x, mut, rhs->rewriteExpr)
  }

and rewriteDecl = decl =>
  switch decl {
  | CoreAst.CoreFuncDecl(f, args, body) => CoreAst.CoreFuncDecl(f, args, rewriteExpr(body))
  | CoreAst.CoreGlobalDecl(x, mut, init) => CoreAst.CoreGlobalDecl(x, mut, rewriteExpr(init))
  | CoreAst.CoreStructDecl(_) => decl
  }

// keep a stack of return types of functions to correctly infer the types for
// function bodies using 'return' expressions
let funcRetTyStack: MutableStack.t<monoTy> = MutableStack.make()

let rec collectCoreExprTypeSubstsWith = (env: Env.t, expr: CoreExpr.t, tau: monoTy): result<
  Subst.t,
  string,
> => {
  collectCoreExprTypeSubsts(env, expr)->Result.flatMap(sig => {
    unify(substMono(sig, tau), substMono(sig, CoreExpr.typeOf(expr)))->Result.map(sig2 =>
      substCompose(sig2, sig)
    )
  })
}

and collectCoreExprTypeSubsts = (env: Env.t, expr: CoreExpr.t): result<Subst.t, string> => {
  open Result

  switch expr {
  | CoreConstExpr(c) =>
    let tau' = Context.freshInstance(constTy(c))
    unify(expr->CoreExpr.typeOf, tau')
  | CoreVarExpr(x) =>
    let x = x.contents
    switch env->Env.get(x.name) {
    | Some(ty) => unify(x.ty, Context.freshInstance(ty))
    | None => Error(`unbound variable: "${x.name}"`)
    }
  | CoreAssignmentExpr(lhs, rhs) => {
      let tau1 = lhs->CoreAst.typeOfExpr
      let tau2 = rhs->CoreAst.typeOfExpr
      collectCoreExprTypeSubsts(env, rhs)->flatMap(sig1 => {
        let sig1Gamma = substEnv(sig1, env)
        let sig1Tau1 = substMono(sig1, tau1)
        collectCoreExprTypeSubstsWith(sig1Gamma, lhs, sig1Tau1)->flatMap(sig2 => {
          let sig21 = substCompose(sig2, sig1)
          let sig21Tau1 = substMono(sig2, sig1Tau1)
          let sig21Tau2 = substMono(sig21, tau2)
          unify(sig21Tau1, sig21Tau2)->map(sig3 => substCompose(sig3, sig21))
        })
      })
    }
  | CoreUnaryOpExpr(tau, op, expr) => {
      let tau1 = CoreExpr.typeOf(expr)
      collectCoreExprTypeSubsts(env, expr)->flatMap(sig => {
        let sigOpTy = substMono(sig, funTy([tau1], tau))
        let tau' = Context.freshInstance(unaryOpTy(op))
        unify(sigOpTy, tau')->map(sig2 => substCompose(sig2, sig))
      })
    }
  | CoreBinOpExpr(tau, a, op, b) =>
    collectCoreExprTypeSubsts(env, a)->flatMap(sigA => {
      let sigAGamma = substEnv(sigA, env)
      collectCoreExprTypeSubsts(sigAGamma, b)->flatMap(sigB => {
        let sigBA = substCompose(sigB, sigA)
        let opTy = substMono(sigBA, funTy([CoreExpr.typeOf(a), CoreExpr.typeOf(b)], tau))
        let tau' = Context.freshInstance(binOpTy(op))
        unify(opTy, tau')->map(sig => substCompose(sig, sigBA))
      })
    })
  | CoreBlockExpr(tau, stmts, lastExpr, _) => {
      open Array

      stmts
      ->reduce(Ok(Map.Int.empty, env), (acc, e) => {
        acc->flatMap(((sig, env)) => {
          collectCoreStmtTypeSubsts(env, e)->Result.map(sig2 => {
            (substCompose(sig2, sig), substEnv(sig2, env))
          })
        })
      })
      ->Result.flatMap(((sig, _)) => {
        switch lastExpr {
        | Some(expr) =>
          collectCoreExprTypeSubsts(substEnv(sig, env), expr)->Result.flatMap(sig2 => {
            let retTy = CoreExpr.typeOf(expr)
            let sig21 = substCompose(sig2, sig)
            unify(substMono(sig21, tau), substMono(sig21, retTy))->Result.map(sig3 =>
              substCompose(sig3, sig21)
            )
          })
        | None => unify(substMono(sig, tau), unitTy)->Result.map(sig2 => substCompose(sig2, sig))
        }
      })
    }
  | CoreLetInExpr(tau, x, e1, e2) => {
      let x = x.contents
      let tau1 = CoreExpr.typeOf(e1)
      collectCoreExprTypeSubsts(env, e1)->flatMap(sig1 => {
        let sig1Gamma = substEnv(sig1, env)
        let sig1Tau1 = substMono(sig1, tau1)
        let sig1Tau = substMono(sig1, tau)
        let sig1Tau1Gen = generalizeTy(sig1Gamma->Env.remove(x.name), sig1Tau1)
        let gammaX = sig1Gamma->Env.add(x.name, sig1Tau1Gen)
        collectCoreExprTypeSubstsWith(gammaX, e2, sig1Tau)->flatMap(sig2 => {
          let sig21 = substCompose(sig2, sig1)
          unify(substMono(sig21, tau1), substMono(sig21, x.ty))->map(sig3 =>
            substCompose(sig3, sig21)
          )
        })
      })
    }
  | CoreIfExpr(tau, e1, e2, e3) =>
    collectCoreExprTypeSubstsWith(env, e1, boolTy)->flatMap(sig1 => {
      let sig1Gamma = substEnv(sig1, env)
      let sig1Tau = substMono(sig1, tau)
      collectCoreExprTypeSubstsWith(sig1Gamma, e2, sig1Tau)->flatMap(sig2 => {
        let sig21Gamma = substEnv(sig2, sig1Gamma)
        let sig21Tau = substMono(sig2, sig1Tau)
        let sig21 = substCompose(sig2, sig1)
        collectCoreExprTypeSubstsWith(sig21Gamma, e3, sig21Tau)->map(sig3 => {
          substCompose(sig3, sig21)
        })
      })
    })

  | CoreFuncExpr(tau, name, args, body) => {
      let tauRet = CoreExpr.typeOf(body)
      funcRetTyStack->MutableStack.push(tauRet)
      let gammaArgs =
        args->Array.reduce(env, (acc, x) => acc->Env.addMono(x.contents.name, x.contents.ty))
      let fTy = funTy(args->Array.map(x => x.contents.ty), tauRet)
      let gammaFArgs =
        name->Option.mapWithDefault(gammaArgs, f =>
          gammaArgs->Env.add(f.contents.name, polyOf(fTy))
        )
      collectCoreExprTypeSubstsWith(gammaFArgs, body, tauRet)->flatMap(sig => {
        let sigFty = substMono(sig, fTy)
        let sigTau = substMono(sig, tau)
        // let sigGamma = substEnv(sig, env)
        // let sigFtyGen = generalizeTy(sigGamma, sigFty)
        unify(sigTau, sigFty)->flatMap(sig2 => {
          let sig21 = substCompose(sig2, sig)

          let _ = funcRetTyStack->MutableStack.pop

          Ok(sig21)
          // TODO: handle polymorphism
          // switch name {
          //   | Some((_, fTy)) => unify(substMono(sig21, fTy), substPoly(sig2, sigFtyGen))->map(sig3 => {
          //     substCompose(sig3, sig21)
          //   })
          //   | None => Ok(sig21)
          // }
        })
      })
    }
  | CoreAppExpr(tau, lhs, args) => {
      let argsTy = args->Array.map(CoreExpr.typeOf)
      let fTy = funTy(argsTy, tau)
      collectCoreExprTypeSubstsWith(env, lhs, fTy)
      ->flatMap(sig1 => {
        args
        ->Array.zip(argsTy)
        ->Array.reduce(Ok((sig1, substEnv(sig1, env))), (acc, (arg, argTy)) => {
          acc->flatMap(((sign, signGamma)) => {
            let signArgTy = substMono(sign, argTy)
            collectCoreExprTypeSubstsWith(signGamma, arg, signArgTy)->map(sig2 => {
              (substCompose(sig2, sign), substEnv(sig2, signGamma))
            })
          })
        })
      })
      ->map(((sig, _)) => sig)
    }
  | CoreWhileExpr(cond, body) => {
      let tauBody = CoreExpr.typeOf(body)
      collectCoreExprTypeSubsts(env, cond)->flatMap(sig1 => {
        let sig1TauBody = substMono(sig1, tauBody)
        let sig1Gamma = substEnv(sig1, env)
        collectCoreExprTypeSubstsWith(sig1Gamma, body, sig1TauBody)->map(sig2 =>
          substCompose(sig2, sig1)
        )
      })
    }
  | CoreReturnExpr(ret) =>
    switch funcRetTyStack->MutableStack.top {
    | Some(funcRetTy) => collectCoreExprTypeSubstsWith(env, ret, funcRetTy)
    | None => Error("'return' used outside of a function")
    }
  | CoreTypeAssertion(expr, originalTy, _) => collectCoreExprTypeSubstsWith(env, expr, originalTy)
  | CoreTupleExpr(exprs) => {
      let res = exprs->Array.reduce(Ok((Subst.empty, env)), (prev, exprN) => {
        prev->flatMap(((sigN, gammaN)) => {
          collectCoreExprTypeSubsts(gammaN, exprN)->flatMap(sig => {
            let nextSig = substCompose(sig, sigN)
            let nextGamma = substEnv(sig, gammaN)
            Ok((nextSig, nextGamma))
          })
        })
      })

      res->map(((sig, _)) => sig)
    }
  | CoreStructExpr(name, attrs) =>
    switch Context.getStruct(name) {
    | Some({attributes}) => {
        let res = attributes->Array.reduce(Ok((env, Subst.empty)), (
          acc,
          {name: attrName, ty: attrTy},
        ) => {
          acc->flatMap(((gammaN, sigN)) => {
            switch attrs->Array.getBy(((n, _)) => n == attrName) {
            | None => Error(`missing field "${attrName}" for struct "${name}"`)
            | Some((_, val)) =>
              collectCoreExprTypeSubstsWith(gammaN, val, attrTy)->flatMap(sig => {
                let gammaN' = substEnv(sig, gammaN)
                let sig' = substCompose(sig, sigN)
                Ok((gammaN', sig'))
              })
            }
          })
        })

        res->map(((_, sig)) => sig)
      }
    | None => Error(`undeclared struct "${name}"`)
    }
  | CoreAttributeAccessExpr(tau, lhs, attr) =>
    collectCoreExprTypeSubsts(env, lhs)->flatMap(sig1 => {
      switch substMono(sig1, lhs->CoreAst.typeOfExpr) {
      | TyConst(structName, _) =>
        switch Context.getStruct(structName) {
        | Some({attributes}) =>
          switch attributes->Array.getBy(({name}) => name === attr) {
          | Some({ty: attrTy}) =>
            unify(substMono(sig1, tau), substMono(sig1, attrTy))->map(sig2 =>
              substCompose(sig2, sig1)
            )
          | None => Error(`attribtue "${attr}" does not exist on struct "${structName}"`)
          }
        | None =>
          Error(
            `tried to access an attribute on non-struct type: ${showMonoTy(
                lhs->CoreAst.typeOfExpr,
              )}`,
          )
        }
      | _ =>
        Error(
          `tried to access an attribute on non-struct type: ${showMonoTy(lhs->CoreAst.typeOfExpr)}`,
        )
      }
    })
  }
}

and collectCoreStmtTypeSubsts = (env: Env.t, stmt: CoreStmt.t): result<Subst.t, string> => {
  switch stmt {
  | CoreExprStmt(expr) => collectCoreExprTypeSubsts(env, expr)
  | CoreLetStmt(_, _, _) =>
    Error(`CoreLetStmt should have been rewritten to CoreLetIn in the inferencer`)
  }
}

let inferCoreExprType = expr => {
  let env = Map.String.empty
  collectCoreExprTypeSubsts(env, expr)->Result.map(subst => {
    (substMono(subst, CoreExpr.typeOf(expr)), subst)
  })
}

let registerDecl = (env, decl: CoreDecl.t): result<(Env.t, Subst.t), string> => {
  switch decl {
  | CoreFuncDecl(f, args, body) =>
    collectCoreExprTypeSubsts(
      env,
      CoreFuncExpr(f.contents.ty, Some(f), args, body),
    )->Result.map(sig => (substEnv(sig, env->Env.addMono(f.contents.name, f.contents.ty)), sig))
  | CoreGlobalDecl(x, _, init) =>
    collectCoreExprTypeSubstsWith(env, init, x.contents.ty)->Result.map(sig => {
      (substEnv(sig, env->Env.addMono(x.contents.name, x.contents.ty)), sig)
    })
  | CoreStructDecl(name, attrs) => {
      Context.declareStruct(Context.makeStruct(name, attrs))
      Ok((env, Subst.empty))
    }
  }
}

let infer = (prog: array<CoreDecl.t>): result<(Env.t, Subst.t), string> => {
  funcRetTyStack->MutableStack.clear

  prog
  ->Array.map(rewriteDecl)
  ->Array.reduce(Ok((Env.empty, Subst.empty)), (acc, decl) => {
    acc->Result.flatMap(((envn, sign)) =>
      registerDecl(envn, decl)->Result.map(((env, sig)) => (env, substCompose(sig, sign)))
    )
  })
}
