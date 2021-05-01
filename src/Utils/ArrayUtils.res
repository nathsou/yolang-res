open Belt

// returns Some(f(arr[n])) for the first n such that f(arr[n]) is not None
let firstSomeBy = (arr: array<'a>, f: 'a => option<'b>): option<'b> => {
  let res = ref(None)
  let i = ref(0)

  while res.contents->Option.isNone && i.contents < arr->Array.length {
    switch f(arr->Array.getExn(i.contents)) {
    | Some(val) => res := Some(val)
    | None => ()
    }

    i := i.contents + 1
  }

  res.contents
}
