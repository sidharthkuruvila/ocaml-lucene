open Lucene_fst
open Monad
let test_fold_left () =
  let module Ops = Ops.Make(Identity) in
  let open Ops in
  let l = ["cat"; "dog"] in
  let init = "horse" in
  let res = fold_left (fun a b -> a ^ b) init l in
  let expected = "horsecatdog" in
  Alcotest.(check string) "Expected function application to be in order of left fold" expected res

let test_fold_right () =
  let module Ops = Ops.Make(Identity) in
  let open Ops in
  let l = ["cat"; "dog"] in
  let init = "horse" in
  let res = fold_right (fun a b -> a ^ b) l init in
  let expected = "dogcathorse" in
  Alcotest.(check string) "Expected function application to be in order of right fold" expected res

let tests = [
  "Apply a left fold", `Quick, test_fold_left;
  "Apply a right fold", `Quick, test_fold_left;
]