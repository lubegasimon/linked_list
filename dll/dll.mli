type 'a dll =
  | Nil
  | Node of { value : 'a; mutable prev : 'a dll; mutable next : 'a dll }

val search : 'a dll -> 'a -> 'a dll
val prepend : 'a dll -> 'a dll -> 'a dll
val insert : 'a dll -> 'a dll -> 'a dll
val remove_last : 'a dll -> 'a dll

(* Helper function to extract values from the node *)
val extract_values : 'a dll -> 'a list

(* Helper function to create a node from a list of values *)
val create_dll : 'a list -> 'a dll
val sort : 'a dll -> 'a dll
val find_first : 'a dll -> 'a dll
val find_last : 'a dll -> 'a dll
val merge : 'a dll -> 'a dll -> 'a dll
val reverse : 'a dll -> 'a dll
