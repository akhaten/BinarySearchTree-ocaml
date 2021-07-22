open Bstree

module IntTree = Make(Int)

let add x t = IntTree.add t x

let _ =
  let m = IntTree.create
  |> add 1
  |> add 2
  |> add 3
  |> add 4
  |> IntTree.max
  in
  assert (m = 4)

let _ =
  let m = IntTree.create
  |> add 4
  |> add 3
  |> add 2
  |> add 1
  |> IntTree.max
  in
  assert (m = 4)


let _ =
  let m = IntTree.create
  |> add 1
  |> add 2
  |> add 3
  |> add 4
  |> IntTree.min
  in
  assert (m = 1)

let _ =
  let m = IntTree.create
  |> add 4
  |> add 3
  |> add 2
  |> add 1
  |> IntTree.min
  in
  assert (m = 1)

let _ =
  let t = IntTree.(remove (add (create) 1) 1) in
  assert (IntTree.is_empty t)

let _ =
  let t = IntTree.create |> add 1 |> add 2 in
  let t' = IntTree.remove t 1 in
  assert (IntTree.get_key t' = 2)

let _ =
  let t = IntTree.create |> add 1 |> add 2 |> add 3 in
  assert (IntTree.get_key t = 1);
  assert (t |> IntTree.get_right |> IntTree.get_key = 2)

let _ =
  let t = IntTree.create |> add 2 |> add 1 |> add 3 in
  assert (IntTree.get_key t = 2);
  assert (t |> IntTree.get_left |> IntTree.get_key = 1);
  assert (t |> IntTree.get_right |> IntTree.get_key = 3)

let _ =
  print_endline "all tests passed !"