open Base
open Stdio

(* dummy main *)
let () = printf "Hello, World!"

(* start of sample code *)
let x = 3
let y = 4
let _z = x + y

(* string *)
let _languages = "OCaml,Perl,C++,C"

(* string *)
let _dashed_languages =
  let language_list = String.split _languages ~on:',' in
  String.concat ~sep:"-" language_list

(* let languages = "OCaml,Perl,C++,C" *)
let _r = String.split ~on:',' _languages

let _dashed_languages =
  let languages = String.split _languages ~on:',' in
  String.concat ~sep:"-" languages

let _area_of_ring inner_radius outer_radius =
  let pi = Float.pi in
  let area_of_circle r = pi *. r *. r in
  area_of_circle outer_radius -. area_of_circle inner_radius

let _aof = _area_of_ring 1. 3.

let _area_of_ring inner_radius outer_radius =
  let _pi = Float.pi in
  let area_of_circle r = _pi *. r *. r in
  let _pi = 0. in
  (* shadowing the outer _pi *)
  area_of_circle outer_radius -. area_of_circle inner_radius

let _ints, _strings = List.unzip [ (1, "one"); (2, "two"); (3, "three") ]

let upcase_first_entry line =
  match String.split ~on:',' line with
  | [] -> assert false (* String.split returns at least one element *)
  | first :: rest -> String.concat ~sep:"," (String.uppercase first :: rest)

let _r = upcase_first_entry "one,two,three"
let _r = upcase_first_entry ""
let _r x = x + 1
let _r = (fun x -> x + 1) 7
let _r = List.map ~f:(fun x -> x + 1) [ 1; 2; 3 ]
let transforms = [ String.uppercase; String.lowercase ]
let _r = List.map ~f:(fun g -> g "Hello World") transforms
let _plusone x = x + 1
let _r = _plusone 3
let _plusone x = x + 1
let _r = (fun x -> x + 1) 7
let _abs_diff x y = abs (x - y)
let _r = _abs_diff 3 4
let _abs_diff x y = abs (x - y)
let _dist_from_3 = _abs_diff 3
let _r = _dist_from_3 8
let _r = _dist_from_3 (-1)
let _abs_diff x y = abs (x - y)
let _abs_diff (x, y) = abs (x - y)
let _r = _abs_diff (3, 4)

let rec _find_first_repeat list =
  match list with
  | [] | [ _ ] -> None (* only zero or one elements, so no repeats *)
  | x :: y :: tl -> if x = y then Some x else _find_first_repeat (y :: tl)

(* mutually recursive functions
   let rec ... and ... *)
let rec is_even x = if x = 0 then true else is_odd (x - 1)
and is_odd x = if x = 0 then false else is_even (x - 1)

let _r = List.map ~f:is_even [ 0; 1; 2; 3; 4; 5 ]
let _r = List.map ~f:is_odd [ 0; 1; 2; 3; 4; 5 ]
let _r = Int.max 3 4 (* prefix *)
let _r = 3 + 4 (* infix  *)
let _r = 3 + 4
let _r = List.map ~f:(( + ) 3) [ 4; 5; 6 ]
let ( +! ) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
let _r = (3, 2) +! (-2, 4)

(* Trying to define a new operator `***`.
      But interpreted as a comment.
   let (***) x y = (x **. y) **. y *)

(* we need some more spaces
   let ( *** ) x y = (x **. y) **. y *)

let _r = Int.max 3 (-4)
let ( |> ) x f = f x
let path = "/usr/bin:/usr/local/bin:/bin:/sbin:/usr/bin"

let _r =
  String.split ~on:':' path
  |> List.dedup_and_sort ~compare:String.compare
  |> List.iter ~f:print_endline

(* the same but more complicated *)
let split_path = String.split ~on:':' path
let deduped_path = List.dedup_and_sort ~compare:String.compare split_path
let _r = List.iter ~f:print_endline deduped_path
let _r = List.iter ~f:print_endline [ "Two"; "lines" ]

(* The precedence of an operator is determined by its first character or two.
   https://v2.ocaml.org/manual/expr.html#ss%3Aprecedence-and-associativity *)

(* Doesn't work because ^> is right-associative.
let ( ^> ) x f = f x
let _r = String.split ~on:':' path
   ^> List.dedup_and_sort ~compare:String.compare
   ^> List.iter ~f:print_endline *)

(* `function` has mattern-matching built-in. *)
let _some_or_zero = function Some x -> x | None -> 0
let _r = List.map ~f:_some_or_zero [ Some 3; None; Some 4 ]
let _some_or_zero num_opt = match num_opt with Some x -> x | None -> 0
let _some_or_default default = function Some x -> x | None -> default
let _r = _some_or_default 3 (Some 5)
let _r = List.map ~f:(_some_or_default 100) [ Some 3; None; Some 4 ]

(* labeled arguments can be provided in any order *)
let _ratio ~num ~denom = Float.of_int num /. Float.of_int denom
let _r = _ratio ~num:3 ~denom:10
let _r = _ratio ~denom:10 ~num:3

(* label punning *)
let num = 3
let denom = 4
let _r = _ratio ~num ~denom

let _r =
  String.split ~on:':' path
  |> List.dedup_and_sort ~compare:String.compare
  |> List.iter ~f:print_endline

let _apply_to_tuple f (first, second) = f ~first ~second
let _apply_to_tuple_2 f (first, second) = f ~second ~first
let divide ~first ~second = first / second

(* When passing labeled functions as arguments, you need to take care
   to be consistent in your ordering of labeled arguments. *)
(* apply_to_tuple_2 divide (3,4) *)

let _apply_to_tuple f (first, second) =
  f ~first ~second _apply_to_tuple divide (3, 4)

(* optional arguments *)
let concat ?sep x y =
  let sep = match sep with None -> "" | Some s -> s in
  x ^ sep ^ y

let _r = concat "foo" "bar" (* without the optional argument *)
let _r = concat ~sep:":" "foo" "bar" (* with the optional argument    *)

(* given the optional argument a default *)
let concat ?(sep = "") x y = x ^ sep ^ y
let _r = concat ~sep:":" "foo" "bar" (* provide the optional argument *)

(* passing Option in explicitly with `?` *)
let _r = concat ?sep:(Some ":") "foo" "bar" (* pass an explicit [Some] *)
let _r = concat "foo" "bar" (* don't provide the optional argument *)

(* passing option in explicitly *)
let _r = concat ?sep:None "foo" "bar" (* explicitly pass `None` *)

(* passing optional arguments *)

(* using uppercase_concat's default value for concat *)
let _uppercase_concat ?(sep = "") a b = concat ~sep (String.uppercase a) b
let _r = _uppercase_concat "foo" "bar"
let _r = _uppercase_concat "foo" "bar" ~sep:":"

(* using concat's default value *)
let _uppercase_concat ?sep a b = concat ?sep (String.uppercase a) b

let _numeric_deriv ~delta ~x ~y ~f =
  let x' = x +. delta in
  let y' = y +. delta in
  let base = f ~x ~y in
  let dx = (f ~x:x' ~y -. base) /. delta in
  let dy = (f ~x ~y:y' -. base) /. delta in
  (dx, dy)

(* This function is applied to arguments in an order different from other calls.This is only allowed when the real type is known.
   let numeric_deriv ~delta ~x ~y ~f =
     let x' = x +. delta in
     let y' = y +. delta in
     let base = f ~x ~y in
     let dx = (f ~y ~x:x' -. base) /. delta in
     let dy = (f ~x ~y:y' -. base) /. delta in
     (dx, dy) *)

let _numeric_deriv' ~delta ~x ~y ~(f : x:float -> y:float -> float) =
  let x' = x +. delta in
  let y' = y +. delta in
  let base = f ~x ~y in
  let dx = (f ~y ~x:x' -. base) /. delta in
  let dy = (f ~x ~y:y' -. base) /. delta in
  (dx, dy)

let colon_concat = concat ~sep:":"
let _r = colon_concat "a" "b"
let prepend_pound = concat "# "
let _r = prepend_pound "a BASH comment"

(* Optional argument has been erased and cannot be used any more.
   It has been erased because the first positional argument after the optional one has been provided.
   let _r = prepend_pound "a BASH comment" ~sep:":" *)

(* Now the optional parameter is after the `x` and it won't be erased. *)
let concat x ?(sep = "") y = x ^ sep ^ y
let prepend_pound = concat "# "
let _r = prepend_pound "a BASH comment"
let _r = prepend_pound "a BASH comment" ~sep:"--- "

(* Separator will be erased again. *)
let _r = concat "a" "b" ~sep:"="

(* if the optional parameter comes last it cannot be erased and will
      lead to a compiler warning because it is not optional any more.
   let concat x y ?(sep = "") = x ^ sep ^ y
   let _r = concat "a" "b" *)
