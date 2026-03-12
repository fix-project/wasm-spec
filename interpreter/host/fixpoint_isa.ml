(*
 * Collection of dummy functions for fix apis
 *)

open WasmRef_Isa.WasmRef_Isa

let is_equal: host =
  Host_func (Abs_host_func (fun (s, vs) -> Some (s, [V_num (ConstInt32 (I32_impl_abs 666l))])))

let is_storage_coupon_api: host =
  Host_func (Abs_host_func (fun (s, vs) -> Some (s, [V_num (ConstInt32 (I32_impl_abs 666l))])))

let get_coupon_lhs: host =
  Host_func (Abs_host_func (fun (s, vs) -> Some (s, vs)))

let get_coupon_rhs: host =
  Host_func (Abs_host_func (fun (s, vs) -> Some (s, vs)))

let create_eq_coupon: host =
  Host_func (Abs_host_func (fun (s, vs) -> Some (s, vs)))

let fixpoint_imports =
  [("is_equal", Func_host (Tf ([T_ref T_ext_ref; T_ref T_ext_ref], [T_num T_i32]), is_equal));
   ("is_storage_coupon_api", Func_host (Tf ([T_ref T_ext_ref], [T_num T_i32]), is_storage_coupon_api));
   ("get_coupon_lhs", Func_host (Tf ([T_ref T_ext_ref], [T_ref T_ext_ref]), get_coupon_lhs));
   ("get_coupon_rhs", Func_host (Tf ([T_ref T_ext_ref], [T_ref T_ext_ref]), get_coupon_rhs));
   ("create_eq_coupon", Func_host (Tf ([T_ref T_ext_ref; T_ref T_ext_ref], [T_ref T_ext_ref]), create_eq_coupon))
  ]

let install_fixpoint_funcs (s : unit s_ext) : (unit s_ext * ((string * v_ext) list)) =
  match s with
  | (S_ext (cls, tabs, mems, globs, elems, datas, _)) ->
    let cl_n = List.length cls in
    let (fixpoint_names, fixpoint_cls) = List.split fixpoint_imports in
    let exp_list = List.mapi (fun i name -> (name, Ext_func (Nat (Z.of_int (cl_n + i))))) fixpoint_names in
    (S_ext (cls@fixpoint_cls, tabs, mems, globs, elems, datas, ()), exp_list)

let install_fixpoint_isa (s : unit s_ext) : (unit s_ext * ((string * v_ext) list)) =
  match s with
  | S_ext (cls, tabs, mems, globs, elems, datas, _) ->
    let (s', exp_funcs) = install_fixpoint_funcs s in
    (s', exp_funcs)
