open Types

(* Taint is local to a function, hence taint is a mapping from function id to the taint... *)

type taint_single =
      | TaintArgument of int
      | TaintGlobal of int

let taint_single_to_string (t : taint_single) : string = match t with
  | TaintArgument i -> Printf.sprintf "l%d" i
  | TaintGlobal i -> Printf.sprintf "g%d" i

module TaintSet = struct
  module T = Set.Make(struct
    type t = taint_single
    let compare t1 t2 = match t1, t2 with
      | TaintArgument l0, TaintArgument l1 -> Stdlib.compare l0 l1
      | TaintArgument _, _ -> 1
      | TaintGlobal g0, TaintGlobal g1 -> Stdlib.compare g0 g1
      | TaintGlobal _, _ -> -1
  end)
  include T
  let to_string (t : t) =
    match T.elements t with
    | [] -> "_"
    | elms -> (String.concat "," (List.map taint_single_to_string elms))
end

module IntMap = Map.Make(struct
    type t = int32
    let compare = Int32.compare
  end)

type taint = TaintSet.t IntMap.t

let no_taint : taint = IntMap.empty

(* Values and operators *)

type ('i32, 'i64, 'f32, 'f64) op =
  I32 of 'i32 * taint | I64 of 'i64 * taint | F32 of 'f32 * taint | F64 of 'f64 * taint

type value = (I32.t, I64.t, F32.t, F64.t) op

let taint_join (t1 : taint) (t2 : taint) : taint =
  List.fold_left (fun acc (k, _) ->
      IntMap.update k (function
          | None -> IntMap.find_opt k t2
          | Some t -> begin match IntMap.find_opt k t2 with
              | Some t' -> Some (TaintSet.union t t')
              | None -> Some t
            end) acc)
    t1
    (IntMap.bindings t1)

let value_set_taint (v : value) (t : taint) = match v with
  | I32 (i, t') -> I32 (i, taint_join t t')
  | I64 (i, t') -> I64 (i, taint_join t t')
  | F32 (z, t') -> F32 (z, taint_join t t')
  | F64 (z, t') -> F64 (z, taint_join t t')

let value_add_taint (v : value) (fidx : int32) (t : TaintSet.t) =
  let update tm = IntMap.update fidx (function
      | None -> Some t
      | Some t' -> Some (TaintSet.union t t')) tm in
  match v with
  | I32 (i, tm) -> I32 (i, update tm)
  | I64 (i, tm) -> I64 (i, update tm)
  | F32 (z, tm) -> F32 (z, update tm)
  | F64 (z, tm) -> F64 (z, update tm)

let value_get_taint (v : value) : taint = match v with
  | I32 (_, t) | I64 (_, t) | F32 (_, t) | F64 (_, t) -> t

let value_get_taint_f (v : value) (fidx : int32) : TaintSet.t =
  let t = value_get_taint v in
  match IntMap.find_opt fidx t with
  | None -> TaintSet.empty
  | Some x -> x


(* Typing *)

let type_of = function
  | I32 _ -> I32Type
  | I64 _ -> I64Type
  | F32 _ -> F32Type
  | F64 _ -> F64Type

let default_value = function
  | I32Type -> I32 (I32.zero, no_taint)
  | I64Type -> I64 (I64.zero, no_taint)
  | F32Type -> F32 (F32.zero, no_taint)
  | F64Type -> F64 (F64.zero, no_taint)


(* Conversion *)

let value_of_bool b = I32 ((if b then 1l else 0l), no_taint)

let string_of_value = function
  | I32 (i, _) -> I32.to_string_s i
  | I64 (i, _) -> I64.to_string_s i
  | F32 (z, _) -> F32.to_string z
  | F64 (z, _) -> F64.to_string z

let string_of_values = function
  | [v] -> string_of_value v
  | vs -> "[" ^ String.concat " " (List.map string_of_value vs) ^ "]"


(* Injection & projection *)

exception Value of value_type

module type ValueType =
sig
  type t
  val to_value : t -> value
  val of_value : value -> t (* raise Value *)
end

module I32Value =
struct
  type t = I32.t
  let to_value i = I32 (i, no_taint)
  let of_value = function I32 (i, _) -> i | _ -> raise (Value I32Type)
end

module I64Value =
struct
  type t = I64.t
  let to_value i = I64 (i, no_taint)
  let of_value = function I64 (i, _) -> i | _ -> raise (Value I64Type)
end

module F32Value =
struct
  type t = F32.t
  let to_value i = F32 (i, no_taint)
  let of_value = function F32 (z, _) -> z | _ -> raise (Value F32Type)
end

module F64Value =
struct
  type t = F64.t
  let to_value i = F64 (i, no_taint)
  let of_value = function F64 (z, _) -> z | _ -> raise (Value F64Type)
end
