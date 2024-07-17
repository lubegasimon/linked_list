type 'a dll =
  | Nil
  | Node of { value : 'a; mutable prev : 'a dll; mutable next : 'a dll }

(* search algorithm:
   LIST-SEARCH (L, k)
    x = L.head
    while x != NIL and x.key != k
      x = x.next
    return x
*)

(* a doubly linked list node represented as node *)
let rec search list key =
  match list with
  | Nil -> Nil
  | Node { value; next; _ } -> if value = key then list else search next key

(* prepend algorithm:

   LIST-PREPEND (L, x) (* where x is the new node *)
    x.next = L.head
    x.prev = NIL
    if L.head != NIL
      L.head.prev = x
    L.head = x
*)
let prepend list x =
  match (list, x) with
  | Nil, _ -> x
  | _, Nil -> list
  | Node old_node, Node new_node ->
      new_node.next <- list;
      old_node.prev <- x;
      x

(* insert algorithm:

   LIST-INSERT (x, y)
     x.next = y.next
     x.prev = y
     if y.next != NIL
       y.next.prev = x
     y.next = x
*)

(* insert x after y and then return the insert node *)
let insert x y =
  match (x, y) with
  | Nil, _ -> y
  | _, Nil -> x
  | Node x', Node y' ->
      let y_next = y'.next in
      y'.next <- x;
      x'.prev <- y;
      x'.next <- y_next;
      (match y_next with Nil -> () | Node next -> next.prev <- x);
      x

(* TODO: use helper functions extract_values and create_dll *)
let remove_last dll =
  match dll with
  | Nil -> Nil
  | Node x -> (
      let head_key = x.value in
      match x.next with
      | Nil -> dll (* dll contains one node *)
      | next ->
          let rec aux next =
            match next with
            | Nil -> dll
            | Node next' ->
                if next'.value <> head_key then
                  match next'.next with
                  | Nil -> (
                      let next_prev = next'.prev in
                      match next_prev with
                      | Nil -> dll
                      | Node prev ->
                          prev.next <- Nil;
                          (* removed the last element by assigning it's prev node's next value to Nil*)
                          dll)
                  | x' -> aux x'
                else dll
          in
          aux next)

(* FIXME: bug! *)
(* let remove_last dll =
   let rec find_last prev curr =
     match curr with
     | Nil -> prev (* list is empty or we've reached the end *)
     | Node curr' -> (
         match curr'.next with
         | Nil -> (
             match prev with
             | Nil -> Nil (* only one node in the list *)
             | Node pre ->
                 pre.next <- Nil (* remove last node *);
                 dll)
         | Node x -> find_last curr x.next)
   in
   find_last Nil dll *)

let rec extract_values = function
  | Nil -> []
  | Node { value; next; _ } -> value :: extract_values next

let rec create_dll = function
  | [] -> Nil
  | x :: xs ->
      let node = Node { value = x; prev = Nil; next = create_dll xs } in
      (match node with
      | Nil -> ()
      | Node n -> (
          match n.next with Nil -> () | Node next -> next.prev <- node));
      node

let sort dll =
  let values = extract_values dll in
  let sorted_values = List.sort compare values in
  create_dll sorted_values

let find_first dll =
  let rec aux curr =
    match curr with
    | Nil -> Nil
    | Node n -> ( match n.prev with Nil -> curr | Node _ -> aux n.prev)
  in
  aux dll

let find_last dll =
  let rec aux curr =
    match curr with
    | Nil -> Nil
    | Node n -> ( match n.next with Nil -> curr | Node _ -> aux n.next)
  in
  aux dll

let merge x y =
  match (x, y) with
  | Nil, Nil -> Nil
  | Nil, y -> y
  | x, Nil -> x
  | Node _, Node _ ->
      let values = extract_values x @ extract_values y in
      create_dll values

let reverse dll =
  match dll with
  | Nil -> Nil
  | Node _ ->
      let values = extract_values dll |> List.rev in
      create_dll values
