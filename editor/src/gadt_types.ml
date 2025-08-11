(* gadt_types.ml - Core data structure for refelction types *)

(* Type equality witnesses *)
type (_, _) type_eq = Refl : ('a, 'a) type_eq

type _ type_tag =
  | IntTag : int type_tag
  | StringTag : string type_tag
  | BoolTag : bool type_tag

let equal_tags : type a b. a type_tag -> b type_tag -> (a, b) type_eq option =
  fun t1 t2 ->
    match t1, t2 with
    | IntTag, IntTag -> Some Refl
    | StringTag, StringTag -> Some Refl
    | BoolTag, BoolTag -> Some Refl
    | _ -> None

(* Safe cast using type equality *)
let safe_cast : type a b. (a, b) type_eq -> a -> b =
  fun Refl x -> x

(* Demo function with correct pattern matching *)
let demo_type_equality () =
  print_endline "5. Type equality witnesses:";
  
  (* Check IntTag = IntTag *)
  begin match equal_tags IntTag IntTag with
  | Some Refl -> 
      print_endline "   IntTag = IntTag ✓";
      (* Now we can safely cast because we have proof they're equal *)
      let x : int = 42 in
      let y : int = safe_cast Refl x in
      Printf.printf "   Cast %d to %d\n" x y
  | None -> 
      print_endline "   IntTag ≠ IntTag ✗"
  end;
  
  (* Check IntTag = StringTag - this will always be None *)
  begin match equal_tags IntTag StringTag with
  | Some _ -> 
      (* This branch is impossible - the types don't match *)
      print_endline "   This can never happen!"
  | None -> 
      print_endline "   IntTag ≠ StringTag ✗ (as expected)"
  end