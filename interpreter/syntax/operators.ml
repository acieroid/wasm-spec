open Source
open Types
open Values
open Ast


let i32_const n = Const (I32 (n.it, no_taint) @@ n.at)
let i64_const n = Const (I64 (n.it, no_taint) @@ n.at)
let f32_const n = Const (F32 (n.it, no_taint) @@ n.at)
let f64_const n = Const (F64 (n.it, no_taint) @@ n.at)

let unreachable = Unreachable
let nop = Nop
let drop = Drop
let select = Select
let block bt es = Block (bt, es)
let loop bt es = Loop (bt, es)
let if_ bt es1 es2 = If (bt, es1, es2)
let br x = Br x
let br_if x = BrIf x
let br_table xs x = BrTable (xs, x)

let return = Return
let call x = Call x
let call_indirect x = CallIndirect x

let local_get x = LocalGet x
let local_set x = LocalSet x
let local_tee x = LocalTee x
let global_get x = GlobalGet x
let global_set x = GlobalSet x

let i32_load align offset = Load {ty = I32Type; align; offset; sz = None}
let i64_load align offset = Load {ty = I64Type; align; offset; sz = None}
let f32_load align offset = Load {ty = F32Type; align; offset; sz = None}
let f64_load align offset = Load {ty = F64Type; align; offset; sz = None}
let i32_load8_s align offset =
  Load {ty = I32Type; align; offset; sz = Some (Pack8, SX)}
let i32_load8_u align offset =
  Load {ty = I32Type; align; offset; sz = Some (Pack8, ZX)}
let i32_load16_s align offset =
  Load {ty = I32Type; align; offset; sz = Some (Pack16, SX)}
let i32_load16_u align offset =
  Load {ty = I32Type; align; offset; sz = Some (Pack16, ZX)}
let i64_load8_s align offset =
  Load {ty = I64Type; align; offset; sz = Some (Pack8, SX)}
let i64_load8_u align offset =
  Load {ty = I64Type; align; offset; sz = Some (Pack8, ZX)}
let i64_load16_s align offset =
  Load {ty = I64Type; align; offset; sz = Some (Pack16, SX)}
let i64_load16_u align offset =
  Load {ty = I64Type; align; offset; sz = Some (Pack16, ZX)}
let i64_load32_s align offset =
  Load {ty = I64Type; align; offset; sz = Some (Pack32, SX)}
let i64_load32_u align offset =
  Load {ty = I64Type; align; offset; sz = Some (Pack32, ZX)}

let i32_store align offset = Store {ty = I32Type; align; offset; sz = None}
let i64_store align offset = Store {ty = I64Type; align; offset; sz = None}
let f32_store align offset = Store {ty = F32Type; align; offset; sz = None}
let f64_store align offset = Store {ty = F64Type; align; offset; sz = None}
let i32_store8 align offset =
  Store {ty = I32Type; align; offset; sz = Some Pack8}
let i32_store16 align offset =
  Store {ty = I32Type; align; offset; sz = Some Pack16}
let i64_store8 align offset =
  Store {ty = I64Type; align; offset; sz = Some Pack8}
let i64_store16 align offset =
  Store {ty = I64Type; align; offset; sz = Some Pack16}
let i64_store32 align offset =
  Store {ty = I64Type; align; offset; sz = Some Pack32}

let i32_clz = Unary (I32 (I32Op.Clz, no_taint))
let i32_ctz = Unary (I32 (I32Op.Ctz, no_taint))
let i32_popcnt = Unary (I32 (I32Op.Popcnt, no_taint))
let i64_clz = Unary (I64 (I64Op.Clz, no_taint))
let i64_ctz = Unary (I64 (I64Op.Ctz, no_taint))
let i64_popcnt = Unary (I64 (I64Op.Popcnt, no_taint))
let f32_neg = Unary (F32 (F32Op.Neg, no_taint))
let f32_abs = Unary (F32 (F32Op.Abs, no_taint))
let f32_sqrt = Unary (F32 (F32Op.Sqrt, no_taint))
let f32_ceil = Unary (F32 (F32Op.Ceil, no_taint))
let f32_floor = Unary (F32 (F32Op.Floor, no_taint))
let f32_trunc = Unary (F32 (F32Op.Trunc, no_taint))
let f32_nearest = Unary (F32 (F32Op.Nearest, no_taint))
let f64_neg = Unary (F64 (F64Op.Neg, no_taint))
let f64_abs = Unary (F64 (F64Op.Abs, no_taint))
let f64_sqrt = Unary (F64 (F64Op.Sqrt, no_taint))
let f64_ceil = Unary (F64 (F64Op.Ceil, no_taint))
let f64_floor = Unary (F64 (F64Op.Floor, no_taint))
let f64_trunc = Unary (F64 (F64Op.Trunc, no_taint))
let f64_nearest = Unary (F64 (F64Op.Nearest, no_taint))

let i32_add = Binary (I32 (I32Op.Add, no_taint))
let i32_sub = Binary (I32 (I32Op.Sub, no_taint))
let i32_mul = Binary (I32 (I32Op.Mul, no_taint))
let i32_div_s = Binary (I32 (I32Op.DivS, no_taint))
let i32_div_u = Binary (I32 (I32Op.DivU, no_taint))
let i32_rem_s = Binary (I32 (I32Op.RemS, no_taint))
let i32_rem_u = Binary (I32 (I32Op.RemU, no_taint))
let i32_and = Binary (I32 (I32Op.And, no_taint))
let i32_or = Binary (I32 (I32Op.Or, no_taint))
let i32_xor = Binary (I32 (I32Op.Xor, no_taint))
let i32_shl = Binary (I32 (I32Op.Shl, no_taint))
let i32_shr_s = Binary (I32 (I32Op.ShrS, no_taint))
let i32_shr_u = Binary (I32 (I32Op.ShrU, no_taint))
let i32_rotl = Binary (I32 (I32Op.Rotl, no_taint))
let i32_rotr = Binary (I32 (I32Op.Rotr, no_taint))
let i64_add = Binary (I64 (I64Op.Add, no_taint))
let i64_sub = Binary (I64 (I64Op.Sub, no_taint))
let i64_mul = Binary (I64 (I64Op.Mul, no_taint))
let i64_div_s = Binary (I64 (I64Op.DivS, no_taint))
let i64_div_u = Binary (I64 (I64Op.DivU, no_taint))
let i64_rem_s = Binary (I64 (I64Op.RemS, no_taint))
let i64_rem_u = Binary (I64 (I64Op.RemU, no_taint))
let i64_and = Binary (I64 (I64Op.And, no_taint))
let i64_or = Binary (I64 (I64Op.Or, no_taint))
let i64_xor = Binary (I64 (I64Op.Xor, no_taint))
let i64_shl = Binary (I64 (I64Op.Shl, no_taint))
let i64_shr_s = Binary (I64 (I64Op.ShrS, no_taint))
let i64_shr_u = Binary (I64 (I64Op.ShrU, no_taint))
let i64_rotl = Binary (I64 (I64Op.Rotl, no_taint))
let i64_rotr = Binary (I64 (I64Op.Rotr, no_taint))
let f32_add = Binary (F32 (F32Op.Add, no_taint))
let f32_sub = Binary (F32 (F32Op.Sub, no_taint))
let f32_mul = Binary (F32 (F32Op.Mul, no_taint))
let f32_div = Binary (F32 (F32Op.Div, no_taint))
let f32_min = Binary (F32 (F32Op.Min, no_taint))
let f32_max = Binary (F32 (F32Op.Max, no_taint))
let f32_copysign = Binary (F32 (F32Op.CopySign, no_taint))
let f64_add = Binary (F64 (F64Op.Add, no_taint))
let f64_sub = Binary (F64 (F64Op.Sub, no_taint))
let f64_mul = Binary (F64 (F64Op.Mul, no_taint))
let f64_div = Binary (F64 (F64Op.Div, no_taint))
let f64_min = Binary (F64 (F64Op.Min, no_taint))
let f64_max = Binary (F64 (F64Op.Max, no_taint))
let f64_copysign = Binary (F64 (F64Op.CopySign, no_taint))

let i32_eqz = Test (I32 (I32Op.Eqz, no_taint))
let i64_eqz = Test (I64 (I64Op.Eqz, no_taint))

let i32_eq = Compare (I32 (I32Op.Eq, no_taint))
let i32_ne = Compare (I32 (I32Op.Ne, no_taint))
let i32_lt_s = Compare (I32 (I32Op.LtS, no_taint))
let i32_lt_u = Compare (I32 (I32Op.LtU, no_taint))
let i32_le_s = Compare (I32 (I32Op.LeS, no_taint))
let i32_le_u = Compare (I32 (I32Op.LeU, no_taint))
let i32_gt_s = Compare (I32 (I32Op.GtS, no_taint))
let i32_gt_u = Compare (I32 (I32Op.GtU, no_taint))
let i32_ge_s = Compare (I32 (I32Op.GeS, no_taint))
let i32_ge_u = Compare (I32 (I32Op.GeU, no_taint))
let i64_eq = Compare (I64 (I64Op.Eq, no_taint))
let i64_ne = Compare (I64 (I64Op.Ne, no_taint))
let i64_lt_s = Compare (I64 (I64Op.LtS, no_taint))
let i64_lt_u = Compare (I64 (I64Op.LtU, no_taint))
let i64_le_s = Compare (I64 (I64Op.LeS, no_taint))
let i64_le_u = Compare (I64 (I64Op.LeU, no_taint))
let i64_gt_s = Compare (I64 (I64Op.GtS, no_taint))
let i64_gt_u = Compare (I64 (I64Op.GtU, no_taint))
let i64_ge_s = Compare (I64 (I64Op.GeS, no_taint))
let i64_ge_u = Compare (I64 (I64Op.GeU, no_taint))
let f32_eq = Compare (F32 (F32Op.Eq, no_taint))
let f32_ne = Compare (F32 (F32Op.Ne, no_taint))
let f32_lt = Compare (F32 (F32Op.Lt, no_taint))
let f32_le = Compare (F32 (F32Op.Le, no_taint))
let f32_gt = Compare (F32 (F32Op.Gt, no_taint))
let f32_ge = Compare (F32 (F32Op.Ge, no_taint))
let f64_eq = Compare (F64 (F64Op.Eq, no_taint))
let f64_ne = Compare (F64 (F64Op.Ne, no_taint))
let f64_lt = Compare (F64 (F64Op.Lt, no_taint))
let f64_le = Compare (F64 (F64Op.Le, no_taint))
let f64_gt = Compare (F64 (F64Op.Gt, no_taint))
let f64_ge = Compare (F64 (F64Op.Ge, no_taint))

let i32_extend8_s = Unary (I32 (I32Op.ExtendS Pack8, no_taint))
let i32_extend16_s = Unary (I32 (I32Op.ExtendS Pack16, no_taint))
let i64_extend8_s = Unary (I64 (I64Op.ExtendS Pack8, no_taint))
let i64_extend16_s = Unary (I64 (I64Op.ExtendS Pack16, no_taint))
let i64_extend32_s = Unary (I64 (I64Op.ExtendS Pack32, no_taint))

let i32_wrap_i64 = Convert (I32 (I32Op.WrapI64, no_taint))
let i32_trunc_f32_s = Convert (I32 (I32Op.TruncSF32, no_taint))
let i32_trunc_f32_u = Convert (I32 (I32Op.TruncUF32, no_taint))
let i32_trunc_f64_s = Convert (I32 (I32Op.TruncSF64, no_taint))
let i32_trunc_f64_u = Convert (I32 (I32Op.TruncUF64, no_taint))
let i32_trunc_sat_f32_s = Convert (I32 (I32Op.TruncSatSF32, no_taint))
let i32_trunc_sat_f32_u = Convert (I32 (I32Op.TruncSatUF32, no_taint))
let i32_trunc_sat_f64_s = Convert (I32 (I32Op.TruncSatSF64, no_taint))
let i32_trunc_sat_f64_u = Convert (I32 (I32Op.TruncSatUF64, no_taint))
let i64_extend_i32_s = Convert (I64 (I64Op.ExtendSI32, no_taint))
let i64_extend_i32_u = Convert (I64 (I64Op.ExtendUI32, no_taint))
let i64_trunc_f32_s = Convert (I64 (I64Op.TruncSF32, no_taint))
let i64_trunc_f32_u = Convert (I64 (I64Op.TruncUF32, no_taint))
let i64_trunc_f64_s = Convert (I64 (I64Op.TruncSF64, no_taint))
let i64_trunc_f64_u = Convert (I64 (I64Op.TruncUF64, no_taint))
let f32_convert_i32_s = Convert (F32 (F32Op.ConvertSI32, no_taint))
let f32_convert_i32_u = Convert (F32 (F32Op.ConvertUI32, no_taint))
let f32_convert_i64_s = Convert (F32 (F32Op.ConvertSI64, no_taint))
let f32_convert_i64_u = Convert (F32 (F32Op.ConvertUI64, no_taint))
let i64_trunc_sat_f32_s = Convert (I64 (I64Op.TruncSatSF32, no_taint))
let i64_trunc_sat_f32_u = Convert (I64 (I64Op.TruncSatUF32, no_taint))
let i64_trunc_sat_f64_s = Convert (I64 (I64Op.TruncSatSF64, no_taint))
let i64_trunc_sat_f64_u = Convert (I64 (I64Op.TruncSatUF64, no_taint))
let f32_demote_f64 = Convert (F32 (F32Op.DemoteF64, no_taint))
let f64_convert_i32_s = Convert (F64 (F64Op.ConvertSI32, no_taint))
let f64_convert_i32_u = Convert (F64 (F64Op.ConvertUI32, no_taint))
let f64_convert_i64_s = Convert (F64 (F64Op.ConvertSI64, no_taint))
let f64_convert_i64_u = Convert (F64 (F64Op.ConvertUI64, no_taint))
let f64_promote_f32 = Convert (F64 (F64Op.PromoteF32, no_taint))
let i32_reinterpret_f32 = Convert (I32 (I32Op.ReinterpretFloat, no_taint))
let i64_reinterpret_f64 = Convert (I64 (I64Op.ReinterpretFloat, no_taint))
let f32_reinterpret_i32 = Convert (F32 (F32Op.ReinterpretInt, no_taint))
let f64_reinterpret_i64 = Convert (F64 (F64Op.ReinterpretInt, no_taint))

let memory_size = MemorySize
let memory_grow = MemoryGrow

