type t = {
  name: string,
  mutable newName: string,
  index: int,
  mutable ty: Types.monoTy,
}

type nameRef = ref<t>
type renameMap = Belt.HashMap.Int.t<nameRef>
