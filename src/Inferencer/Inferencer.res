open Belt
open Core
open Types
open Subst
open Unification

let constTy = (c: Ast.Expr.Const.t): polyTy => {
  let ty = switch c {
  | Ast.Expr.Const.U32Const(_) => u32Ty
  | Ast.Expr.Const.BoolConst(_) => boolTy
  | Ast.Expr.Const.UnitConst => unitTy
  }

  polyOf(ty)
}

let binOpTy = (op: Token.BinOp.t): polyTy => {
  open Token.BinOp
  switch op {
  | Plus => polyOf(funTy([u32Ty, u32Ty, u32Ty]))
  | Sub => polyOf(funTy([u32Ty, u32Ty, u32Ty]))
  | Mult => polyOf(funTy([u32Ty, u32Ty, u32Ty]))
  | Div => polyOf(funTy([u32Ty, u32Ty, u32Ty]))
  | Mod => polyOf(funTy([u32Ty, u32Ty, u32Ty]))
  | Eq => ([0], funTy([TyVar(0), TyVar(0), boolTy])) // 'a -> 'a -> bool
  | Neq => ([0], funTy([TyVar(0), TyVar(0), boolTy])) // 'a -> 'a -> bool
  | Lss => polyOf(funTy([u32Ty, u32Ty, boolTy]))
  | Leq => polyOf(funTy([u32Ty, u32Ty, boolTy]))
  | Gtr => polyOf(funTy([u32Ty, u32Ty, boolTy]))
  | Geq => polyOf(funTy([u32Ty, u32Ty, boolTy]))
  }
}

// rewrite
// { let a = 3; let b = 7; a * b }
// into
// let a = 3 in let b = 7 in a * b
let rec rewriteExpr = expr => {
  open CoreAst

  switch expr {
  | CoreBlockExpr(tau, stmts, lastExpr) => {
      let rec aux = stmts =>
        switch stmts {
        | list{CoreLetStmt(x, _, e), ...tl} =>
          Some(
            CoreLetInExpr(
              tau,
              x,
              rewriteExpr(e),
              aux(tl)->Option.mapWithDefault(CoreConstExpr(unitTy, Ast.Expr.Const.UnitConst), x =>
                x
              ),
            ),
          )
        | list{stmt, ...tl} => Some(CoreBlockExpr(tau, [rewriteStmt(stmt)], aux(tl)))
        | list{} => lastExpr->Option.map(rewriteExpr)
        }

      CoreBlockExpr(tau, [], aux(stmts->List.fromArray))
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
  }

let rec collectCoreExprTypeSubstsWith = (env: Env.t, expr: CoreExpr.t, tau: monoTy): result<
  Subst.t,
  string,
> => {
  collectCoreExprTypeSubsts(env, expr)->Result.flatMap(sig => {
    unify(substMono(sig, tau), substMono(sig, CoreExpr.tyVarOf(expr)))->Result.map(sig2 =>
      substCompose(sig2, sig)
    )
  })
}
and collectCoreExprTypeSubsts = (env: Env.t, expr: CoreExpr.t): result<Subst.t, string> => {
  open Result

  switch expr {
  | CoreConstExpr(tau, c) =>
    let tau' = Context.freshInstance(constTy(c))
    unify(tau, tau')
  | CoreVarExpr(tau, x) =>
    switch env->Env.get(x) {
    | Some(ty) => unify(tau, Context.freshInstance(ty))
    | None => Error(`unbound variable: "${x}"`)
    }
  | CoreAssignmentExpr(x, val) =>
    switch env->Env.get(x) {
    | Some(ty) =>
      collectCoreExprTypeSubstsWith(env, val, Context.freshInstance(ty))->flatMap(sig1 =>
        unify(substMono(sig1, CoreExpr.tyVarOf(expr)), unitTy)->map(sig2 =>
          substCompose(sig2, sig1)
        )
      )
    | None => Error(`unbound variable: "${x}"`)
    }
  | CoreBinOpExpr(tau, a, op, b) =>
    collectCoreExprTypeSubsts(env, a)->flatMap(sigA => {
      let sigAGamma = substEnv(sigA, env)
      collectCoreExprTypeSubsts(sigAGamma, b)->flatMap(sigB => {
        let sigBA = substCompose(sigB, sigA)
        let opTy = substMono(sigBA, funTy([CoreExpr.tyVarOf(a), CoreExpr.tyVarOf(b), tau]))
        let tau' = Context.freshInstance(binOpTy(op))
        unify(opTy, tau')->map(sig => substCompose(sig, sigBA))
      })
    })
  | CoreBlockExpr(tau, stmts, lastExpr) => {
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
            let retTy = CoreExpr.tyVarOf(expr)
            let sig21 = substCompose(sig2, sig)
            unify(substMono(sig21, tau), substMono(sig21, retTy))->Result.map(sig3 =>
              substCompose(sig3, sig21)
            )
          })
        | None => unify(substMono(sig, tau), unitTy)->Result.map(sig2 => substCompose(sig2, sig))
        }
      })
    }
  | CoreLetInExpr(tau, (x, tauX), e1, e2) => {
      let tau1 = CoreExpr.tyVarOf(e1)
      collectCoreExprTypeSubsts(env, e1)->flatMap(sig1 => {
        let sig1Gamma = substEnv(sig1, env)
        let sig1Tau1 = substMono(sig1, tau1)
        let sig1Tau = substMono(sig1, tau)
        let sig1Tau1Gen = generalizeTy(sig1Gamma->Env.remove(x), sig1Tau1)
        let gammaX = sig1Gamma->Env.add(x, sig1Tau1Gen)
        collectCoreExprTypeSubstsWith(gammaX, e2, sig1Tau)->flatMap(sig2 => {
          let sig21 = substCompose(sig2, sig1)
          unify(substMono(sig21, tau1), substMono(sig21, tauX))->map(sig3 =>
            substCompose(sig3, sig21)
          )
        })
      })
    }
  // | CoreLetRecInExpr(tau, (f, tauF), (x, tauX), eVal, eIn) => {
  //     let tauVal = tyVarOf(eVal)
  //     let fTy = funTy([tauX, tauVal])
  //     let gammaFX = env->Env.add(x, polyOf(tauX))->Env.add(f, polyOf(fTy))
  //     collectCoreExprTypeSubsts(gammaFX, eVal)->flatMap(sig1 => {
  //       let sig1FTy = substMono(sig1, fTy)
  //       let sig1Tau = substMono(sig1, tau)
  //       let sig1Gamma = substEnv(sig1, env)
  //       let sig1FtyGen = generalizeTy(sig1Gamma, sig1FTy)
  //       let sig1GammaFGen = sig1Gamma->Env.add(f, sig1FtyGen)
  //       collectCoreExprTypeSubstsWith(sig1GammaFGen, eIn, sig1Tau)->flatMap(sig2 => {
  //         let sig21 = substCompose(sig2, sig1)
  //         unify(substMono(sig21, tauF), substMono(sig2, sig1FTy))->map(sig3 =>
  //           substCompose(sig3, sig21)
  //         )
  //       })
  //     })
  //   }
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
  | CoreFuncExpr(tau, args, body) => {
      let tauRet = CoreExpr.tyVarOf(body)
      let gammaArgs = args->Array.reduce(env, (acc, (x, xTy)) => acc->Env.addMono(x, xTy))
      let fTy = funTy(Array.concat(args->Array.map(((_, ty)) => ty), [tauRet]))
      collectCoreExprTypeSubsts(gammaArgs, body)->flatMap(sig => {
        let sigfTy = substMono(sig, fTy)
        let sigTau = substMono(sig, tau)
        unify(sigTau, sigfTy)->map(sig2 => substCompose(sig2, sig))
      })
    }
  | CoreAppExpr(tau, lhs, args) => {
      let argsTy = args->Array.map(CoreExpr.tyVarOf)
      let fTy = funTy(Array.concat(argsTy, [tau]))
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
  }
}

and collectCoreStmtTypeSubsts = (env: Env.t, stmt: CoreStmt.t): result<Subst.t, string> => {
  switch stmt {
  | CoreExprStmt(expr) => collectCoreExprTypeSubsts(env, expr)
  | CoreLetStmt(_, _, _) =>
    Js.Exn.raiseError(`CoreLetStmt should have been rewritten to CoreLetIn in the inferencer`)
  }
}

let inferCoreExprType = expr => {
  let env = Map.String.empty
  collectCoreExprTypeSubsts(env, expr)->Result.map(subst => {
    (substMono(subst, CoreExpr.tyVarOf(expr)), subst)
  })
}

let registerDecl = (env, decl: CoreDecl.t): result<(Env.t, Subst.t), string> => {
  switch decl {
  | CoreFuncDecl((f, fTy), args, body) => {
      let tau = Context.freshTyVar()
      collectCoreExprTypeSubstsWith(env, CoreFuncExpr(tau, args, body), fTy)->Result.map(sig => (
        substEnv(sig, env->Env.addMono(f, tau)),
        sig,
      ))
    }
  }
}

let infer = (prog: array<CoreDecl.t>): result<(Env.t, Subst.t), string> => {
  prog
  ->Array.map(rewriteDecl)
  ->Array.reduce(Ok((Env.empty, Subst.empty)), (acc, decl) => {
    acc->Result.flatMap(((envn, sign)) =>
      registerDecl(envn, decl)->Result.map(((env, sig)) => (env, substCompose(sig, sign)))
    )
  })
}
