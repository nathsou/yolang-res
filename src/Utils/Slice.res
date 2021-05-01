type t<'a> = (array<'a>, int, int)

let make = (arr): t<'a> => (arr, 0, arr->Belt.Array.length)

let fromString = str => {
  make(str->Js.String.castToArrayLike->Js.Array.fromMap(c => c->String.get(0)))
}

let step = (~count=1, (arr, start, end): t<'a>): t<'a> => {
  (arr, Js.Math.min_int(start + count, end), end)
}

let head = ((arr, start, _): t<'a>) => arr->Belt.Array.get(start)

let tail = slice => slice->step(~count=1)

let length = ((_, start, end): t<'a>) => end - start

let isEmpty = ((_, start, end): t<'a>) => start == end

let toArray = ((arr, start, end): t<'a>) => arr->Js.Array.slice(~start, ~end_=end)
