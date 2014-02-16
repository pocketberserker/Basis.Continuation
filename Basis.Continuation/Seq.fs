namespace Basis.Continuation

module Seq =

  let (|Empty|Cons|) xs =
    if xs |> Seq.isEmpty then Empty
    else Cons ((Seq.head xs), (Seq.skip 1 xs))

  let rec fold f state xs cont =
    match xs with
    | Empty -> cont state
    | Cons(x, xs) -> f state x (fun v -> fold f v xs cont)

  let rec foldBack f xs state cont =
    match xs with
    | Empty -> cont state
    | Cons(x, xs) -> foldBack f xs state (fun state -> f x state cont)

  let cons x xs cont = seq { yield x; yield! xs } |> cont

  let map f xs cont = foldBack (fun x acc k -> cons (f x) acc k) xs Seq.empty cont

  let concat xss cont =
    foldBack (fun xs acc k -> acc |> Seq.append xs |> k) xss Seq.empty cont

  let collect f xs cont =
    fold (fun acc x k -> Seq.append acc (f x k)) Seq.empty xs cont

  let take count (xs: _ seq) cont =
    let xs =
      if count < 0 then invalidArg "count" "input should be nonnegative number."
      elif count = 0 then Seq.empty
      else
        seq {
          use e = xs.GetEnumerator()
          for _ in 0 .. count - 1 do
            if not (e.MoveNext()) then
              raise <| System.InvalidOperationException "not enough elements."
            yield e.Current }
    cont xs
