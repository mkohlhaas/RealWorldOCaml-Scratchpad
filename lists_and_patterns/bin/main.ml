open Base
open Base.Poly
open Fn
open Stdio
(* open Core_bench (* uncomment to run benchmark tests *) *)

let () = print_endline ""
let _r = [ 1; 2; 3 ]
let empty = []
let _r = 3 :: empty
let _r = "three" :: empty
let l = [ 1; 2; 3 ]
let _m = 0 :: l
let rec sum l = match l with [] -> 0 | hd :: tl -> hd + sum tl
let _r = sum [ 1; 2; 3 ]
let _r = sum []

(* let rec drop_value l to_drop =
       match l with
       | [] -> []
       | to_drop :: tl -> drop_value tl to_drop   (* no pattern-matching on to_drop *)
       | hd :: tl -> hd :: drop_value tl to_drop  (* unused *)

   Line 5, characters 7-15:
   Warning 11 [redundant-case]: this match case is unused. *)

let rec drop_value l to_drop =
  match l with
  | [] -> []
  | hd :: tl ->
      let new_tl = drop_value tl to_drop in
      if hd = to_drop then new_tl else hd :: new_tl

(* using guards *)
let rec drop_value' l to_drop =
  match l with
  | [] -> []
  | hd :: tl when hd = to_drop -> drop_value' tl to_drop
  | hd :: tl -> hd :: drop_value' tl to_drop

let rec _drop_zero l =
  match l with
  | [] -> []
  | 0 :: tl -> _drop_zero tl
  | hd :: tl -> hd :: _drop_zero tl

let _r = drop_value [ 1; 2; 3 ] 2
let _r = drop_value' [ 1; 2; 3 ] 2

let _plus_one_match x =
  match x with
  | 0 -> 1
  | 1 -> 2
  | 2 -> 3
  | 3 -> 4
  | 4 -> 5
  | 5 -> 6
  | _ -> x + 1

let _plus_one_if x =
  if x = 0 then 1
  else if x = 1 then 2
  else if x = 2 then 3
  else if x = 3 then 4
  else if x = 4 then 5
  else if x = 5 then 6
  else x + 1

(* pattern-matching is fast *)
(* uncomment to run benchmark tests *)

(* let _r =
   [
     Bench.Test.create ~name:"plus_one_match" (fun () -> _plus_one_match 10);
     Bench.Test.create ~name:"plus_one_if" (fun () -> _plus_one_if 10);
   ]
   |> Bench.bench *)

(* Estimated testing time 20s (2 benchmarks x 10s). Change using -quota SECS.
   ┌────────────────┬──────────┐
   │ Name           │ Time/Run │
   ├────────────────┼──────────┤
   │ plus_one_match │  34.86ns │
   │ plus_one_if    │  54.89ns │
   └────────────────┴──────────┘ *)

let rec _sum_if l =
  if List.is_empty l then 0 else List.hd_exn l + _sum_if (List.tl_exn l)

(* uncomment to run benchmark tests *)
(* let _r =
   let _numbers = List.range 0 1000 in
     [ Bench.Test.create ~name:"sum_if" (fun () -> _sum_if _numbers);
       Bench.Test.create ~name:"sum"    (fun () -> sum _numbers) ]
     |> Bench.bench *)

(* Estimated testing time 20s (2 benchmarks x 10s). Change using -quota SECS.
   ┌────────┬──────────┐
   │ Name   │ Time/Run │
   ├────────┼──────────┤
   │ sum_if │  62.00us │
   │ sum    │  17.99us │
   └────────┴──────────┘ *)

(* let rec drop_zero l =
       match l with
       | [] -> []
       | 0  :: tl -> drop_zero tl

   Lines 2-4, characters 5-31:
   Warning 8 [partial-match]: this pattern-matching is not exhaustive.
   Here is an example of a case that is not matched:
   1::_ *)

let max_widths header rows =
  let lengths l = List.map ~f:String.length l in
  List.fold rows ~init:(lengths header) ~f:(fun acc row ->
      List.map2_exn ~f:Int.max acc (lengths row))

let pad s length = s ^ String.make (length - String.length s) ' '
let _r = pad "hello" 10 (* "hello     " *)

let render_row row widths =
  let padded = List.map2_exn row widths ~f:pad in
  "| " ^ String.concat ~sep:" | " padded ^ " |"

(* - : string = "| Hello      | World           |" *)
let _r = render_row [ "Hello"; "World" ] [ 10; 15 ]

let render_separator widths =
  let pieces = List.map widths ~f:(fun w -> String.make w '-') in
  "|-" ^ String.concat ~sep:"-+-" pieces ^ "-|"

let _r = render_separator [ 3; 6; 2 ] (* "|-----+--------+----|" *)

let render_table header rows =
  let widths = max_widths header rows in
  String.concat ~sep:"\n"
    (render_separator widths :: render_row header widths
     :: render_separator widths
     :: List.map rows ~f:(fun row -> render_row row widths)
    @ [ render_separator widths ])

let _r =
  print_endline
    (render_table
       [ "language"; "architect"; "first release" ]
       [
         [ "Lisp"; "John McCarthy"; "1958" ];
         [ "C"; "Dennis Ritchie"; "1969" ];
         [ "ML"; "Robin Milner"; "1973" ];
         [ "OCaml"; "Xavier Leroy"; "1996" ];
       ])

let _r = List.map ~f:String.length [ "Hello"; "World!" ]
let _r = List.map2_exn ~f:Int.max [ 1; 2; 3 ] [ 3; 2; 1 ]

(* let _r = List.map2_exn ~f:Int.max [1;2;3] [3;2;1;0] *)
(* Exception: (Invalid_argument "length mismatch in map2_exn: 3 <> 4"). *)

let _r = List.fold ~init:0 ~f:( + ) [ 1; 2; 3; 4 ]
let _r = List.fold ~init:[] ~f:(fun acc hd -> hd :: acc) [ 1; 2; 3; 4 ]
let _s = "." ^ "." ^ "." ^ "." ^ "." ^ "." ^ "."
let _s = String.concat [ "."; "."; "."; "."; "."; "."; "." ]
let _r = List.reduce ~f:( + ) [ 1; 2; 3; 4; 5 ]
let _r = List.reduce ~f:( + ) []
let _r = List.filter ~f:(fun x -> x % 2 = 0) [ 1; 2; 3; 4; 5 ]

let extensions filenames =
  List.filter_map filenames ~f:(fun fname ->
      match String.rsplit2 ~on:'.' fname with
      | None | Some ("", _) -> None
      | Some (_, ext) -> Some ext)
  |> List.dedup_and_sort ~compare:String.compare

let _r = extensions [ "foo.c"; "foo.ml"; "bar.ml"; "bar.mli" ]

let is_ocaml_source s =
  match String.rsplit2 s ~on:'.' with
  | Some (_, ("ml" | "mli")) -> true
  | _ -> false

let _ml_files, _other_files =
  List.partition_tf
    [ "foo.c"; "foo.ml"; "bar.ml"; "bar.mli" ]
    ~f:is_ocaml_source

let _r = List.append [ 1; 2; 3 ] [ 4; 5; 6 ]
let _r = [ 1; 2; 3 ] @ [ 4; 5; 6 ]
let _r = List.concat [ [ 1; 2 ]; [ 3; 4; 5 ]; [ 6 ]; [] ]

(* # #require "core_unix.sys_unix" *)

module Sys = Core.Sys
module Filename = Core.Filename

let rec _ls_rec s =
  if Sys_unix.is_file_exn ~follow_symlinks:true s then [ s ]
  else
    Sys_unix.ls_dir s
    |> List.map ~f:(fun sub -> _ls_rec (Filename.concat s sub))
    |> List.concat

let rec _ls_rec s =
  if Sys_unix.is_file_exn ~follow_symlinks:true s then [ s ]
  else
    Sys_unix.ls_dir s
    |> List.concat_map ~f:(fun sub -> _ls_rec (Filename.concat s sub))

let rec length = function [] -> 0 | _ :: tl -> 1 + length tl
let _r = length [ 1; 2; 3 ]
let make_list n = List.init n ~f:id
let _r = length (make_list 10)

(* let _r = length (make_list 10_000_000) *)
(* Stack overflow during evaluation (looping recursion?). *)

let rec length_plus_n l n =
  match l with [] -> n | _ :: tl -> length_plus_n tl (n + 1)

let length l = length_plus_n l 0
let _r = length [ 1; 2; 3; 4 ]
let _r = length (make_list 10_000_000)

let rec _remove_sequential_duplicates list =
  match list with
  | [] -> []
  | [ x ] -> [ x ]
  | first :: second :: tl ->
      if first = second then _remove_sequential_duplicates (second :: tl)
      else first :: _remove_sequential_duplicates (second :: tl)

(* using as patterns *)
let rec _remove_sequential_duplicates = function
  | [] as l -> l
  | [ _ ] as l -> l
  | first :: (second :: _ as tl) ->
      if first = second then _remove_sequential_duplicates tl
      else first :: _remove_sequential_duplicates tl

(* using or pattern *)
let rec _remove_sequential_duplicates list =
  match list with
  | ([] | [ _ ]) as l -> l
  | first :: (second :: _ as tl) ->
      if first = second then _remove_sequential_duplicates tl
      else first :: _remove_sequential_duplicates tl

(* using guards *)
let rec _remove_sequential_duplicates list =
  match list with
  | ([] | [ _ ]) as l -> l
  | first :: (second :: _ as tl) when first = second ->
      _remove_sequential_duplicates tl
  | first :: tl -> first :: _remove_sequential_duplicates tl

(* works because of `open Base.Poly` *)
let _r = "foo" = "bar"
let _r = 3 = 4
let _r = [ 1; 2; 3 ] = [ 1; 2; 3 ]
let _r = _remove_sequential_duplicates [ 1; 2; 2; 3; 4; 3; 3 ]
(* - : int list = [1; 2; 3; 4; 3] *)

let _r = _remove_sequential_duplicates [ "one"; "two"; "two"; "two"; "three" ]
(* - : string list = ["one"; "two"; "three"] *)

(* let _r = (fun x -> x + 1) = (fun x -> x + 1) *)
(* Exception: (Invalid_argument "compare: functional value"). *)

(* this pattern-matching is not exhaustive.Here is an example of a case that is not matched:_::_(However, some guarded clause may match this value.)
   let rec count_some list =
       match list with
       | [] -> 0
       | x :: tl when Option.is_none x -> count_some tl
       | x :: tl when Option.is_some x -> 1 + count_some tl *)

(* let rec count_some list =
    match list with
    | [] -> 0
    | x :: tl when Option.is_none x -> count_some tl
    | x :: tl when Option.is_some x -> 1 + count_some tl
    | x :: tl -> -1 (* unreachable *) *)

let rec _count_some l =
  match l with
  | [] -> 0
  | x :: tl when Option.is_none x -> _count_some tl
  | _ :: tl -> 1 + _count_some tl

let rec _count_some l =
  match l with
  | [] -> 0
  | None :: tl -> _count_some tl
  | Some _ :: tl -> 1 + _count_some tl

let _r = _count_some [ Some 3; None; Some 4 ]
(* - : int = 2 *)

let _count_some l = List.count ~f:Option.is_some l
