namespace Basis.Continuation.Tests

open NUnit.Framework
open FsUnit
open FsCheck
open FsCheck.NUnit

module ContSeq = Basis.Continuation.Seq

[<TestFixture>]
module SeqTest =

  [<Test>]
  let ``Continuation.Seq.fold should be equal to Seq.fold`` () =
    check <| fun (xs: int list) ->
      ContSeq.fold (fun acc x k -> acc + x |> k) 0 xs id = Seq.fold (+) 0 xs

  [<Test>]
  let ``Continuation.Seq.foldBack should be equal to List.foldBack`` () =
    check <| fun (xs: int list) ->
      ContSeq.foldBack (fun acc x k -> acc + x |> k) xs 0 id = List.foldBack (+) xs 0

  [<Test>]
  let ``Continuation.Seq.map should be equal to Seq.map`` () =
    check <| fun (xs: int list) ->
      let a = ContSeq.map ((*) 2) xs id |> Seq.toList
      let b = Seq.map ((*) 2) xs |> Seq.toList
      a = b

  [<Test>]
  let ``Continuation.Seq.concat should be equal to Seq.concat`` () =
    check <| fun (xss: int list list) ->
      let a = ContSeq.concat xss id |> Seq.toList
      let b = Seq.concat xss |> Seq.toList
      a = b

  [<Test>]
  let ``Continuation.Seq.collect should be equal to Seq.collect`` () =
    check <| fun (xs: int list) ->
      let a = ContSeq.collect (fun x k -> seq { yield x; yield -x } |> k) xs id |> Seq.toList
      let b = Seq.collect (fun x -> seq { yield x; yield -x }) xs |> Seq.toList
      a = b
