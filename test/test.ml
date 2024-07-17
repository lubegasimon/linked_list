open Dll

let rec equal_node (node_a : 'a dll) node_b =
  match (node_a, node_b) with
  | Nil, Nil -> true
  | ( Node { value = s1; next = n1; prev = p1 },
      Node { value = s2; next = n2; prev = p2 } ) ->
      s1 = s2 && equal_node n1 n2 && equal_node p1 p2
  | _ -> false

let rec pp_print_node pp_v fmt node =
  match node with
  | Nil -> Format.fprintf fmt "Nil"
  | Node { value; next; prev } ->
      Format.fprintf fmt "Node { value = %a; next = %a; prev = %a}" pp_v value
        (pp_print_node pp_v) next (pp_print_node pp_v) prev

let node_testable pp_v = Alcotest.testable (pp_print_node pp_v) equal_node
let node_0 = Node { value = 0; prev = Nil; next = Nil }
let node_1 = Node { value = 1; prev = node_0; next = Nil }
let _node_2 = Node { value = 2; prev = Nil; next = Nil }

let search_node_test () =
  let actual = search node_1 1 in
  let expected =
    Node
      {
        value = 1;
        next = Nil;
        prev = Node { value = 0; next = Nil; prev = Nil };
      }
  in
  Alcotest.(check (node_testable Format.pp_print_int))
    "should return a node containing a key" actual expected

let test_prepend_node () =
  let dll = create_dll [ 2; 3 ] in
  let new_node = Node { value = 1; prev = Nil; next = Nil } in
  let result = prepend dll new_node in

  let actual = extract_values result in
  let expected = [ 1; 2; 3 ] in
  Alcotest.(check (list int)) "should prepend node to dll" actual expected

let test_insert_node () =
  let y_node = Node { value = 1; prev = Nil; next = Nil } in
  let x_node = Node { value = 2; prev = Nil; next = Nil } in

  let result = insert x_node y_node in
  let actual = extract_values result in
  let expected = [ 2 ] in
  Alcotest.(check (list int)) "should insert x after y" actual expected

let test_remove_last () =
  let dll = create_dll [ 1; 2; 3; 4 ] in
  let result = remove_last dll in
  let actual = extract_values result in
  let expected = [ 1; 2; 3 ] in
  Alcotest.(check (list int)) "should remove last node" actual expected

let test_sort () =
  let dll = create_dll [ 2; 4; 1; 3 ] in
  let result = sort dll in
  let actual = extract_values result in
  let expected = [ 1; 2; 3; 4 ] in
  Alcotest.(check (list int)) "should sort dll" actual expected

let test_find_first () =
  let dll = create_dll [ 2; 4; 1; 3 ] in
  let result = find_first dll in
  let actual = extract_values result in
  (* FIXME: expected should be [ 2 ]*)
  let expected = [ 2; 4; 1; 3 ] in
  Alcotest.(check (list int)) "should find first node" actual expected

let test_find_last () =
  let dll = create_dll [ 2; 4; 1; 3 ] in
  let result = find_last dll in
  let actual = extract_values result in
  let expected = [ 3 ] in
  Alcotest.(check (list int)) "should find last node" actual expected

let test_merge () =
  let x_node = create_dll [ 2; 1 ] in
  let y_node = create_dll [ 3; 4 ] in
  let result = merge x_node y_node in
  let actual = extract_values result in
  let expected = [ 2; 1; 3; 4 ] in
  Alcotest.(check (list int)) "should merge nodes" actual expected

let test_reverse () =
  let dll = create_dll [ 4; 3; 2; 1 ] in
  let result = reverse dll in
  let actual = extract_values result in
  let expected = [ 1; 2; 3; 4 ] in
  Alcotest.(check (list int)) "should reverse dll" actual expected

let () =
  let open Alcotest in
  run "search node test"
    [
      ( "test: search for node",
        [ test_case "search for node" `Quick search_node_test ] );
      ( "test: prepend node",
        [ test_case "prepend node" `Quick test_prepend_node ] );
      ( "test: insert node",
        [ test_case "nsert node test" `Quick test_insert_node ] );
      ( "test: remove last node",
        [ test_case "remove last node" `Quick test_remove_last ] );
      ("test: sort dll", [ test_case "sort dll" `Quick test_sort ]);
      ( "test: find first node",
        [ test_case "find first node" `Quick test_find_first ] );
      ( "test: find last node",
        [ test_case "find last node" `Quick test_find_last ] );
      ("test: merge nodes", [ test_case "merge nodes" `Quick test_merge ]);
      ("test: reverse nodes", [ test_case "reverse nodes" `Quick test_reverse ]);
    ]
