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

// returns the largest index i such that pred(arr[i]) is true
let getReverseIndexBy = (arr: array<'a>, pred: 'a => bool): option<int> => {
  let res = ref(None)
  let i = ref(arr->Array.length - 1)

  while res.contents->Option.isNone && i.contents >= 0 {
    if pred(arr->Array.getExn(i.contents)) {
      res := Some(i.contents)
    } else {
      i := i.contents - 1
    }
  }

  res.contents
}

let getReverseBy = (arr: array<'a>, pred: 'a => bool): option<'a> => {
  let res = ref(None)
  let i = ref(arr->Array.length - 1)

  while res.contents->Option.isNone && i.contents >= 0 {
    let elem = arr->Array.getExn(i.contents)
    if pred(elem) {
      res := Some(elem)
    } else {
      i := i.contents - 1
    }
  }

  res.contents
}
