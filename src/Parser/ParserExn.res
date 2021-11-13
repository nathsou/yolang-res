exception InvalidCharacter(char)

let show = (exn, next) =>
  switch exn {
  | InvalidCharacter(c) => "invalid character: '" ++ String.make(1, c) ++ "'"
  | _ => next(exn)
  }
