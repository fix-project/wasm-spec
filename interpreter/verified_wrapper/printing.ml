open WasmRef_Isa.WasmRef_Isa

let pp_t_num fmt = function
  | T_i32 -> Format.fprintf fmt "T_i32"
  | T_f32 -> Format.fprintf fmt "T_f32"
  | T_i64 -> Format.fprintf fmt "T_i64"
  | T_f64 -> Format.fprintf fmt "T_f64"

let pp_t_vec fmt = function
  | T_v128 -> Format.fprintf fmt "T_v128"

let pp_t_ref fmt = function
  | T_func_ref -> Format.fprintf fmt "T_func_ref"
  | T_ext_ref -> Format.fprintf fmt "T_ext_ref"

let pp_t fmt = function
  | T_num t -> Format.fprintf fmt "(T_num "; pp_t_num fmt t; Format.fprintf fmt ")"
  | T_vec t -> Format.fprintf fmt "(T_vec "; pp_t_vec fmt t; Format.fprintf fmt ")"
  | T_ref t -> Format.fprintf fmt "(T_ref "; pp_t_ref fmt t; Format.fprintf fmt ")"
  | _ -> failwith "Illformed T_bot"

let pp_list pp_elem fmt lst =
  let rec aux = function
    | [] -> ()
    | [x] -> pp_elem fmt x
    | x :: xs ->
        pp_elem fmt x;
        Format.fprintf fmt ",\n";
        aux xs
  in
  Format.fprintf fmt "[";
  aux lst;
  Format.fprintf fmt "]"

let pp_list_no_newline pp_elem fmt lst =
  let rec aux = function
    | [] -> ()
    | [x] -> pp_elem fmt x
    | x :: xs ->
        pp_elem fmt x;
        Format.fprintf fmt ", ";
        aux xs
  in
  Format.fprintf fmt "[";
  aux lst;
  Format.fprintf fmt "]"

let pp_t_list fmt lst =
  pp_list_no_newline pp_t fmt lst

let pp_nat fmt (Nat t) =
  Format.fprintf fmt "%ld" (Z.to_int32 t)

let pp_tf fmt (Tf (lhs, rhs)) =
  Format.fprintf fmt "(";
  pp_t_list fmt lhs;
  Format.fprintf fmt " _> ";
  pp_t_list fmt rhs;
  Format.fprintf fmt ")"

let pp_inst_ext fmt (Inst_ext (types, funcs, tabs, mems, globs, elems, datas, _)) =
  Format.fprintf fmt "\\<lparr>";
  Format.fprintf fmt "types = ";
  pp_list pp_tf fmt types;
  Format.fprintf fmt ",\n";

  Format.fprintf fmt "funcs = ";
  pp_list_no_newline pp_nat fmt funcs;
  Format.fprintf fmt ",\n";

  Format.fprintf fmt "tabs = ";
  pp_list_no_newline pp_nat fmt tabs;
  Format.fprintf fmt ",\n";

  Format.fprintf fmt "mems = ";
  pp_list_no_newline pp_nat fmt mems;
  Format.fprintf fmt ",\n";

  Format.fprintf fmt "globs = ";
  pp_list_no_newline pp_nat fmt globs;
  Format.fprintf fmt ",\n";

  Format.fprintf fmt "elems = ";
  pp_list_no_newline pp_nat fmt elems;
  Format.fprintf fmt ",\n";

  Format.fprintf fmt "datas = ";
  pp_list_no_newline pp_nat fmt datas;
  Format.fprintf fmt "\n";

  Format.fprintf fmt "\\<rparr>"

let pp_tb fmt = function
  | Tbf (Nat i) -> Format.fprintf fmt "(Tbf %ld)" (Z.to_int32 i)
  | Tbv (Some t) -> Format.fprintf fmt "(Tbv (Some "; pp_t fmt t; Format.fprintf fmt "))"
  | Tbv (None) -> Format.fprintf fmt "(Tbv None)"

let pp_tpnum fmt = function
  | Tp_i8 -> Format.fprintf fmt "Tp_i8"
  | Tp_i16 -> Format.fprintf fmt "Tp_i16"
  | Tp_i32 -> Format.fprintf fmt "Tp_i32"

let pp_sx fmt = function
  | S -> Format.fprintf fmt "S"
  | U -> Format.fprintf fmt "U"

let pp_i32 fmt (I32_impl_abs i) =
  Format.fprintf fmt "(Abs_i32 %ld)" i

let pp_i64 fmt (I64_impl_abs i) =
  Format.fprintf fmt "(Abs_i64 %Ld)" i

let pp_v_num fmt = function
  | ConstInt32 c -> Format.fprintf fmt "(ConstInt32 "; pp_i32 fmt c; Format.fprintf fmt ")"
  | ConstInt64 c -> Format.fprintf fmt "(ConstInt64 "; pp_i64 fmt c; Format.fprintf fmt ")"
  | ConstFloat32 c -> failwith "Unimplemented"
  | ConstFloat64 c -> failwith "Unimplemented"

let pp_v_vec fmt (v : v_vec) : unit =
  failwith "Unimplemented"

let pp_v_ref fmt = function
  | ConstNull tref -> Format.fprintf fmt "(ConstNull "; pp_t_ref fmt tref; Format.fprintf fmt ")"
  | (ConstRefExtern (Host_ref x)) -> Format.fprintf fmt "(ConstRefExtern (Host_ref %ld))" x
  | _ -> failwith "Unimplemented"

let pp_v fmt = function
  | V_num n -> Format.fprintf fmt "(V_num "; pp_v_num fmt n; Format.fprintf fmt ")"
  | V_vec n -> Format.fprintf fmt "(V_vec "; pp_v_vec fmt n; Format.fprintf fmt ")"
  | V_ref n -> Format.fprintf fmt "(V_ref "; pp_v_ref fmt n; Format.fprintf fmt ")"

let pp_testop fmt = function
  | Eqz -> Format.fprintf fmt "Eqz"

let pp_relop_i fmt = function
  | Eq -> Format.fprintf fmt "Eq"
  | Ne -> Format.fprintf fmt "Ne"
  | (Lt s) -> Format.fprintf fmt "(Lt "; pp_sx fmt s; Format.fprintf fmt ")"
  | (Gt s) -> Format.fprintf fmt "(Gt "; pp_sx fmt s; Format.fprintf fmt ")"
  | (Le s) -> Format.fprintf fmt "(Le "; pp_sx fmt s; Format.fprintf fmt ")"
  | (Ge s) -> Format.fprintf fmt "(Ge "; pp_sx fmt s; Format.fprintf fmt ")"

let pp_relop_f fmt = function
  | Eqf -> Format.fprintf fmt "Eqf"
  | Nef -> Format.fprintf fmt "Nef"
  | Ltf -> Format.fprintf fmt "Ltf"
  | Gtf -> Format.fprintf fmt "Gtf"
  | Lef -> Format.fprintf fmt "Lef"
  | Gef -> Format.fprintf fmt "Gef"

let pp_relop fmt = function
  | Relop_i i -> Format.fprintf fmt "(Relop_i "; pp_relop_i fmt i; Format.fprintf fmt ")"
  | Relop_f f -> Format.fprintf fmt "(Relop_f "; pp_relop_f fmt f; Format.fprintf fmt ")"

let pp_unop_i fmt = function
  | Clz -> Format.fprintf fmt "Clz"
  | Ctz -> Format.fprintf fmt "Ctz"
  | Popcnt -> Format.fprintf fmt "Popcnt"

let pp_unop_f fmt = function
  | Neg -> Format.fprintf fmt "Neg"
  | Abs -> Format.fprintf fmt "Abs"
  | Ceil -> Format.fprintf fmt "Ceil"
  | Floor -> Format.fprintf fmt "Floor"
  | Trunc -> Format.fprintf fmt "Trunc"
  | Nearest -> Format.fprintf fmt "Nearest"
  | Sqrt -> Format.fprintf fmt "Sqrt"

let pp_unop fmt = function
  | Unop_i i -> Format.fprintf fmt "(Unop_i "; pp_unop_i fmt i; Format.fprintf fmt ")"
  | Unop_f f -> Format.fprintf fmt "(Unop_f "; pp_unop_f fmt f; Format.fprintf fmt ")"
  | Extend_s tpnum -> Format.fprintf fmt "(Extend_s "; pp_tpnum fmt tpnum; Format.fprintf fmt ")"

let pp_binop_i fmt = function
  | Add -> Format.fprintf fmt "Add"
  | Sub -> Format.fprintf fmt "Sub"
  | Mul -> Format.fprintf fmt "Mul"
  | And -> Format.fprintf fmt "And"
  | Or -> Format.fprintf fmt "Or"
  | Xor -> Format.fprintf fmt "Xor"
  | Shl -> Format.fprintf fmt "Shl"
  | Rotl -> Format.fprintf fmt "Rotl"
  | Rotr -> Format.fprintf fmt "Rotr"
  | Div s -> Format.fprintf fmt "(Div "; pp_sx fmt s; Format.fprintf fmt ")"
  | Rem s -> Format.fprintf fmt "(Rem "; pp_sx fmt s; Format.fprintf fmt ")"
  | Shr s -> Format.fprintf fmt "(Shr "; pp_sx fmt s; Format.fprintf fmt ")"

let pp_binop_f fmt = function
  | Addf -> Format.fprintf fmt "Addf"
  | Subf -> Format.fprintf fmt "Subf"
  | Mulf -> Format.fprintf fmt "Mulf"
  | Divf -> Format.fprintf fmt "Divf"
  | Min -> Format.fprintf fmt "Min"
  | Max -> Format.fprintf fmt "Max"
  | Copysign -> Format.fprintf fmt "Copysign"

let pp_binop fmt = function
  | Binop_i i -> Format.fprintf fmt "(Binop_i "; pp_binop_i fmt i; Format.fprintf fmt ")"
  | Binop_f f -> Format.fprintf fmt "(Binop_f "; pp_binop_f fmt f; Format.fprintf fmt ")"

let rec pp_be fmt = function
  | Unreachable -> Format.fprintf fmt "Unreachable"
  | Nop -> Format.fprintf fmt "Nop"
  | Drop -> Format.fprintf fmt "Drop"
  | Block (tb, sublst) ->
      Format.fprintf fmt "(Block ";
      pp_tb fmt tb;
      Format.fprintf fmt "\n";
      pp_list pp_be fmt sublst;
      Format.fprintf fmt ")"
  | Loop (tb, sublst) ->
      Format.fprintf fmt "(Loop ";
      pp_tb fmt tb;
      Format.fprintf fmt "\n";
      pp_list pp_be fmt sublst;
      Format.fprintf fmt ")"
  | If (tb, llst, rlst) ->
      Format.fprintf fmt "(If ";
      pp_tb fmt tb;
      Format.fprintf fmt "\n";
      pp_list pp_be fmt llst;
      Format.fprintf fmt "\n";
      pp_list pp_be fmt rlst;
      Format.fprintf fmt ")"
  | Br i ->
      Format.fprintf fmt "(Br ";
      pp_nat fmt i;
      Format.fprintf fmt ")"
  | Br_if i ->
      Format.fprintf fmt "(Br_if ";
      pp_nat fmt i;
      Format.fprintf fmt ")"
  | Return -> Format.fprintf fmt "Return"
  | Call i ->
      Format.fprintf fmt "(Call ";
      pp_nat fmt i;
      Format.fprintf fmt ")"
  | Local_get i ->
      Format.fprintf fmt "(Local_get ";
      pp_nat fmt i;
      Format.fprintf fmt ")"
  | Local_set i ->
      Format.fprintf fmt "(Local_set ";
      pp_nat fmt i;
      Format.fprintf fmt ")"
  | Local_tee i ->
      Format.fprintf fmt "(Local_tee ";
      pp_nat fmt i;
      Format.fprintf fmt ")"
  | Table_get i ->
      Format.fprintf fmt "(Table_get ";
      pp_nat fmt i;
      Format.fprintf fmt ")"
  | Table_set i ->
      Format.fprintf fmt "(Table_set ";
      pp_nat fmt i;
      Format.fprintf fmt ")"
  | Table_size i ->
      Format.fprintf fmt "(Table_size ";
      pp_nat fmt i;
      Format.fprintf fmt ")"
  | Table_grow i ->
      Format.fprintf fmt "(Table_grow ";
      pp_nat fmt i;
      Format.fprintf fmt ")"
  | EConstNum vnum ->
      Format.fprintf fmt "(EConstNum ";
      pp_v_num fmt vnum;
      Format.fprintf fmt ")";
  | Unop (tnum, op) ->
      Format.fprintf fmt "(Unop ";
      pp_t_num fmt tnum;
      Format.fprintf fmt " ";
      pp_unop fmt op;
      Format.fprintf fmt ")"
  | Binop (tnum, op) ->
      Format.fprintf fmt "(Binop ";
      pp_t_num fmt tnum;
      Format.fprintf fmt " ";
      pp_binop fmt op;
      Format.fprintf fmt ")"
  | Testop (tnum, op) ->
      Format.fprintf fmt "(Testop ";
      pp_t_num fmt tnum;
      Format.fprintf fmt " ";
      pp_testop fmt op;
      Format.fprintf fmt ")"
  | Relop (tnum, op) ->
      Format.fprintf fmt "(Relop ";
      pp_t_num fmt tnum;
      Format.fprintf fmt " ";
      pp_relop fmt op;
      Format.fprintf fmt ")"
  | Ref_null tref ->
      Format.fprintf fmt "(Ref_null ";
      pp_t_ref fmt tref;
      Format.fprintf fmt ")"
  | Ref_is_null -> Format.fprintf fmt "Ref_is_null"
  | Ref_func i ->
      Format.fprintf fmt "(Ref_func ";
      pp_nat fmt i;
      Format.fprintf fmt ")"
  | _ -> failwith "Unimplemented"

let pp_host fmt = function
  | Host_func host ->
      Format.fprintf fmt "(Host_func somethinghere)"
  | Host_ref i ->
      Format.fprintf fmt "(Host_ref %ld)" i

let pp_cl_native fmt inst_ext tf t be =
  Format.fprintf fmt "(Func_native \n";
  pp_inst_ext fmt inst_ext;
  Format.fprintf fmt "\n";
  pp_tf fmt tf;
  Format.fprintf fmt "\n";
  pp_list_no_newline pp_t fmt t;
  Format.fprintf fmt "\n";
  pp_list pp_be fmt be;
  Format.fprintf fmt ")"

let rec pp_cl_list fmt (import_list: unit module_import_ext list) = function
  | [] -> ()
  | x :: xs ->
      let new_import_list =
         match x with
         | Func_native (inst_ext, tf, t, be) -> pp_cl_native fmt inst_ext tf t be; import_list
         | Func_host (tf, host) ->
             Format.fprintf fmt "(Func_host ";
             pp_tf fmt tf;
             Format.fprintf fmt " ";
             match host with
             | Host_func host ->
                 (match import_list with
                 | [] -> failwith "Out of import entries"
                 | i :: is ->
                     match i with
                     | Module_import_ext (s1, s2, (Imp_func n), _) ->
                         Format.fprintf fmt "(Host_func %s_%s)),\n" s1 s2; is
                     | _ -> failwith "Incorrect import type")
             | Host_ref i -> Format.fprintf fmt "(Host_ref %ld)),\n" i; import_list
         in
      pp_cl_list fmt new_import_list xs

let pp_limit_t fmt = function
  | Limit_t_ext ( min, max, _ ) ->
      Format.fprintf fmt "\\<lparr> l_min = ";
      pp_nat fmt min;
      Format.fprintf fmt ", l_max = ";
      match max with
      | Some max ->
          Format.fprintf fmt "(Some ";
          pp_nat fmt max;
          Format.fprintf fmt ")"
      | None ->
          Format.fprintf fmt "None"
          ;
      Format.fprintf fmt "\\<rparr>"

let pp_tab_t fmt = function
  | T_tab ( limit, ref ) ->
      Format.fprintf fmt "(T_tab ";
      pp_limit_t fmt limit;
      Format.fprintf fmt " ";
      pp_t_ref fmt ref;
      Format.fprintf fmt ")"

let pp_tabinst fmt (tab, vreflst) =
  Format.fprintf fmt "(";
  pp_tab_t fmt tab;
  Format.fprintf fmt ", ";
  pp_list pp_v_ref fmt vreflst;
  Format.fprintf fmt ")"

let pp_s_ext fmt import_list = function
  | S_ext ( funcs, tabs, mems, globs, elems, datas, _ ) ->
      Format.fprintf fmt "\\<lparr> funcs = \n";
      Format.fprintf fmt "[";
      pp_cl_list fmt import_list funcs;
      Format.fprintf fmt "]";
      Format.fprintf fmt ",\n";
      Format.fprintf fmt "tabs = ";
      pp_list pp_tabinst fmt tabs;
      Format.fprintf fmt ",\n";
      Format.fprintf fmt "mems = [],\n";
      Format.fprintf fmt "globs = [],\n";
      Format.fprintf fmt "elems = [],\n";
      Format.fprintf fmt "datas = []\n";
      Format.fprintf fmt "\\<rparr>\n"
