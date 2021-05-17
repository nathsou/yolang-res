exception InvalidTypeConversion(Types.monoTy)
exception InvalidFunctionSignature(Types.monoTy)
exception VariableNotFound(string)
exception EmptyFunctionStack
exception CannotReassignImmutableValue(string)
exception FunctionNotFound(string)
exception Unimplemented(string)
exception UnsupportedGlobalInitializer(Core.CoreExpr.t)
exception InvalidTypeAssertion(Types.monoTy, Types.monoTy)
exception DerefUsedOutsideOfUnsafeBlock
exception UndeclaredStruct(string)
exception InvalidAttributeAccess(string, Types.monoTy)
exception StructTypeNotMatched(Types.monoTy)
exception AmibguousStruct(Types.Attributes.t, array<Context.Struct.t>)

let show = exn =>
  switch exn {
  | InvalidTypeConversion(ty) => `unsupported type: ${Types.showMonoTy(ty)}`
  | InvalidFunctionSignature(ty) => `invalid function signature: ${Types.showMonoTy(ty)}`
  | VariableNotFound(x) => `variable "${x}" not found`
  | EmptyFunctionStack => "Function stack is empty"
  | CannotReassignImmutableValue(x) => `cannot mutate "${x}" as it is behind an immutable binding`
  | FunctionNotFound(f) => `no function named "${f}" found`
  | Unimplemented(message) => `unimplemented: ${message}`
  | UnsupportedGlobalInitializer(expr) =>
    `unsupported global initializer: ${Core.CoreExpr.show(expr)}`
  | InvalidTypeAssertion(a, b) =>
    `invalid type assertion from ${Types.showMonoTy(a)} to ${Types.showMonoTy(b)}`
  | DerefUsedOutsideOfUnsafeBlock => `the deref operator can only be used in an unsafe block`
  | Types.Size.UnkownTypeSize(ty) => `unknown type size: ${Types.showMonoTy(ty)}`
  | UndeclaredStruct(name) => `undeclared struct "${name}"`
  | InvalidAttributeAccess(attr, ty) => `${attr} does not exist for type "${Types.showMonoTy(ty)}"`
  | StructTypeNotMatched(ty) => `No struct declaration matches type ${Types.showMonoTy(ty)}`
  | AmibguousStruct(attrs, matches) =>
    Inferencer.StructMatching.ambiguousMatchesError(attrs, matches)
  | Core.CoreAst.UndeclaredIdentifer(name) => `identifier "${name}" not found`
  | Core.CoreAst.UnexpectedDeclarationInImplBlock(decl) =>
    `unexpected declaration in impl block: ${Core.CoreDecl.show(decl)}`
  | Core.CoreAst.UnreachableLetStmt => "unreachable: let statement found in Core.fromStmt"
  | _ => "unexpected compiler exception"
  }
