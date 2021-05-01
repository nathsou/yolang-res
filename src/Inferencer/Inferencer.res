open Belt
open Core
open Types
open Subst
open Unification

let constTy = (c: Expr.Const.t): polyTy => {
  let ty = switch c {
  | Expr.Const.IntConst(_) => u32Ty
  | Expr.Const.BoolConst(_) => boolTy
  | Expr.Const.UnitConst => unitTy
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

let rec collectCoreExprTypeSubstsWith = (env: Env.t, expr: Core.t, tau: monoTy): result<
  Subst.t,
  string,
> => {
  collectCoreExprTypeSubsts(env, expr)->Result.flatMap(sig => {
    unify(substMono(sig, tau), substMono(sig, tyVarOf(expr)))->Result.map(sig2 =>
      substCompose(sig2, sig)
    )
  })
}
and collectCoreExprTypeSubsts = (env: Env.t, expr: Core.t): result<Subst.t, string> => {
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
  | CoreBinOpExpr(tau, a, op, b) =>
    collectCoreExprTypeSubsts(env, a)->flatMap(sigA => {
      let sigAGamma = substEnv(sigA, env)
      collectCoreExprTypeSubsts(sigAGamma, b)->flatMap(sigB => {
        let sigBA = substCompose(sigB, sigA)
        let opTy = substMono(sigBA, funTy([tyVarOf(a), tyVarOf(b), tau]))
        let tau' = Context.freshInstance(binOpTy(op))
        unify(opTy, tau')->map(sig => substCompose(sig, sigBA))
      })
    })
  | CoreBlockExpr(tau, exprs) => {
      open Array

      exprs
      ->reduce(Ok(Map.Int.empty, env), (acc, e) => {
        acc->flatMap(((sig, env)) => {
          collectCoreExprTypeSubsts(env, e)->Result.map(sig2 => {
            (substCompose(sig2, sig), substEnv(sig2, env))
          })
        })
      })
      ->Result.flatMap(((sig, _)) => {
        let lastTau = switch exprs->get(exprs->length - 1) {
        | None => unitTy
        | Some(e) => tyVarOf(e)
        }

        unify(tau, lastTau)->Result.map(sig2 => substCompose(sig2, sig))
      })
    }
  | CoreLetInExpr(tau, (x, tauX), e1, e2) => {
      let tau1 = tyVarOf(e1)
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
      let tauRet = tyVarOf(body)
      let gammaArgs = args->Array.reduce(env, (acc, (x, xTy)) => acc->Env.addMono(x, xTy))
      let fTy = funTy(Array.concat(args->Array.map(((_, ty)) => ty), [tauRet]))
      collectCoreExprTypeSubsts(gammaArgs, body)->flatMap(sig => {
        let sigfTy = substMono(sig, fTy)
        let sigTau = substMono(sig, tau)
        unify(sigTau, sigfTy)->map(sig2 => substCompose(sig2, sig))
      })
    }
  | CoreAppExpr(tau, lhs, args) => {
      let argsTy = args->Array.map(tyVarOf)
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

let inferCoreExprType = expr => {
  let env = Map.String.empty
  collectCoreExprTypeSubsts(env, expr)->Result.map(subst => {
    (substMono(subst, tyVarOf(expr)), subst)
  })
}
