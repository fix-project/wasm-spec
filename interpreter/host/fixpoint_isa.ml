(*
 * Collection of dummy functions for fix apis
 *)

open WasmRef_Isa.WasmRef_Isa

let ref_ref_to_i32: host =
  Host_func (Abs_host_func (fun (s, vs) -> Some (s, [V_num (ConstInt32 (I32_impl_abs 666l))])))

let ref_to_i32: host =
  Host_func (Abs_host_func (fun (s, vs) -> Some (s, [V_num (ConstInt32 (I32_impl_abs 666l))])))

let ref_to_ref: host =
  Host_func (Abs_host_func (fun (s, vs) -> Some (s, vs)))

let ref_ref_to_ref: host =
  Host_func (Abs_host_func (fun (s, vs) -> Some (s, vs)))

let ref_i32_to_ref: host =
  Host_func (Abs_host_func (fun (s, vs) -> Some (s, vs)))

let fixpoint_imports =
  [("is_equal", Func_host (Tf ([T_ref T_ext_ref; T_ref T_ext_ref], [T_num T_i32]), ref_ref_to_i32));
   ("is_storage_coupon", Func_host (Tf ([T_ref T_ext_ref], [T_num T_i32]), ref_to_i32));
   ("is_force_coupon", Func_host (Tf ([T_ref T_ext_ref], [T_num T_i32]), ref_to_i32));
   ("is_eq_coupon", Func_host (Tf ([T_ref T_ext_ref], [T_num T_i32]), ref_to_i32));
   ("is_eval_coupon", Func_host (Tf ([T_ref T_ext_ref], [T_num T_i32]), ref_to_i32));
   ("is_apply_coupon", Func_host (Tf ([T_ref T_ext_ref], [T_num T_i32]), ref_to_i32));
   ("is_think_coupon", Func_host (Tf ([T_ref T_ext_ref], [T_num T_i32]), ref_to_i32));
   ("create_application_thunk", Func_host (Tf ([T_ref T_ext_ref], [T_ref T_ext_ref]), ref_to_ref));
   ("create_strict_encode", Func_host (Tf ([T_ref T_ext_ref], [T_ref T_ext_ref]), ref_to_ref));
   ("create_shallow_encode", Func_host (Tf ([T_ref T_ext_ref], [T_ref T_ext_ref]), ref_to_ref));
   ("get_coupon_lhs", Func_host (Tf ([T_ref T_ext_ref], [T_ref T_ext_ref]), ref_to_ref));
   ("get_coupon_rhs", Func_host (Tf ([T_ref T_ext_ref], [T_ref T_ext_ref]), ref_to_ref));
   ("create_eq_coupon", Func_host (Tf ([T_ref T_ext_ref; T_ref T_ext_ref], [T_ref T_ext_ref]), ref_ref_to_ref));
   ("create_eval_coupon", Func_host (Tf ([T_ref T_ext_ref; T_ref T_ext_ref], [T_ref T_ext_ref]), ref_ref_to_ref));
   ("create_think_coupon", Func_host (Tf ([T_ref T_ext_ref; T_ref T_ext_ref], [T_ref T_ext_ref]), ref_ref_to_ref));
   ("create_force_coupon", Func_host (Tf ([T_ref T_ext_ref; T_ref T_ext_ref], [T_ref T_ext_ref]), ref_ref_to_ref));
   ("get_tree_size", Func_host (Tf ([T_ref T_ext_ref], [T_num T_i32]), ref_to_i32));
   ("get_tree_data", Func_host (Tf ([T_ref T_ext_ref; T_num T_i32], [T_ref T_ext_ref]), ref_i32_to_ref));
   ("is_blob_obj", Func_host (Tf ([T_ref T_ext_ref], [T_num T_i32]), ref_to_i32));
   ("is_data", Func_host (Tf ([T_ref T_ext_ref], [T_num T_i32]), ref_to_i32));
   ("is_object", Func_host (Tf ([T_ref T_ext_ref], [T_num T_i32]), ref_to_i32))
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
